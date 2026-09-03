# =============================================================================
# Suicide Prevention Overview-of-Reviews Dashboard
# =============================================================================
# Gap-map style table built from SPO_GRADE_Certainty.xlsx. Rows = one row
# per unique RefID + Intervention + Time + Moderator combo. Columns = the
# Outcome domains in the data. Each cell shows effect direction/magnitude
# (color bar) + GRADE certainty (badge). Hover a cell for a quick summary;
# click it for the full GRADE breakdown, narrative interpretation, and
# applicability notes in a right-hand sidebar.
#
# HOW EFFECT COLOR IS DETERMINED:
#   Each cell is classified into exactly three categories -- Benefit, Harm,
#   or No effect. No magnitude tiers (no "small"/"large" split), and no
#   conversion between OR and SMD is needed for this, since direction alone
#   is enough to sort into the three categories:
#     - SMD:  positive value  = Benefit, negative value = Harm
#     - OR:   value < 1       = Benefit, value > 1      = Harm
#   (both confirmed against every row's narrative interpretation in this file)
#
#   A cell is "No effect" whenever its reported 95% CI includes the null
#   value (1 for OR, 0 for SMD) -- i.e. the estimate isn't statistically
#   distinguishable from no effect -- rather than relying on direction
#   alone. This matched the narrative wording far better than direction-
#   only would (e.g. "OR = 1.19, 95% CI 0.33-4.35" is "No effect" because
#   the CI spans 1, matching "may not differ ... very uncertain").
#
#   Rows where `Moderator` is filled in are subgroup/effect-modification
#   comparisons, not a review's headline finding -- the row label flags
#   these ("Subgroup: ...") so the color can be weighed accordingly.
# =============================================================================


# Packages ----

library(shiny)
library(reactable)
library(rio)
library(here)
library(tidyverse)
library(htmltools)
library(glue)


# Configuration ----

# Columns (besides Outcome) that together define one unique table ROW.
ROW_GROUP_VARS <- c("RefID", "Intervention", "Time", "Moderator")

# Prefix each row's intervention label with its RefID (e.g. "12: School-
# based..."). Useful while cross-checking rows against source reviews;
# flip to FALSE for a cleaner label once row groupings are finalized.
SHOW_REFID_IN_LABEL <- TRUE

# unlikely-to-collide separator for encoding a cell's effect/certainty/
# estimate into a single reactable value string
CELL_SEP <- "\u241F"

## Color palette ----

CERTAINTY_LEVELS <- c("Very Low", "Low", "Moderate", "High")
CERTAINTY_COLORS <- c(
  "Very Low" = "#8D1D58",
  "Low"      = "#8D1D58",
  "Moderate" = "#004F6E",
  "High"     = "#004F6E"
)
CERTAINTY_FILLED <- c("Very Low" = TRUE, "Low" = FALSE, "Moderate" = FALSE, "High" = TRUE)

EFFECT_LABELS <- c("Harm", "No effect", "Benefit")
# UO comms colors; no reds in the brand palette, so Harm borrows a warm neutral.
EFFECT_COLORS <- c(
  "Harm"      = "#F5A604", #C0392B
  "No effect" = "#A2AAAD",
  "Benefit"   = "#007030"
)

# Shared design tokens for the app chrome (title, legend, sidebar, tooltip)
COLORS <- list(
  text        = "#000000",
  text_muted  = "#4D5859",
  text_light  = "#A2AAAD",
  border      = "#E6E6E6",
  background_light = "#F8F2E8",
  white       = "#ffffff",
  subgroup    = "#53C0D8"
)


# Load data ----

SPO_DATA_PATH   <- here("data", "SPO_GRADE_Certainty.xlsx")
LINKS_DATA_PATH <- here("data", "spo_review_pdf_links.xlsx")

walk(
  c(SPO_DATA_PATH, LINKS_DATA_PATH),
  ~ if (!file.exists(.x)) stop("Data file not found at: ", .x)
)

raw_df <- import(SPO_DATA_PATH) %>% as_tibble()
names(raw_df) <- str_trim(names(raw_df))
raw_df <- raw_df %>% mutate(across(where(is.character), str_trim))

# assumes both files share a "RefID" column -- adjust `by` if yours differs
links_df <- import(LINKS_DATA_PATH) %>% as_tibble()
raw_df <- left_join(raw_df, links_df, by = "RefID")


# Helper functions ----

na_blank <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | x == "", "\u2014", x)  # em dash
}

not_blank <- function(x) {
  x <- as.character(x)
  !is.na(x) & x != "" & x != "NA"
}

# strips stray straight double-quote characters some Excel exports leave
# around free-text fields (e.g. `"at postintervention"`)
clean_txt <- function(x) {
  x <- as.character(x)
  x <- str_remove_all(x, '^"+|"+$')
  str_trim(x)
}

# A Moderator value counts as a real subgroup only if it's present and
# isn't a placeholder like "None" -- single source of truth so the row
# label and the detail sidebar always agree on what counts as "real".
is_real_subgroup <- function(x) {
  not_blank(x) & str_to_lower(x) != "none"
}


# Prepare estimates ----
# Metric-aware (OR vs. SMD) effect classification, using CI-based
# significance -- see header comment for the full rationale.

prep <- raw_df %>%
  mutate(
    Time_clean = clean_txt(Time),
    Comparator_clean = str_replace_all(Comparator, "[\r\n]+", "; "),
    
    metric = case_when(
      str_detect(Estimate, "^(Contrast\\s+)?OR")  ~ "OR",
      str_detect(Estimate, "^(Contrast\\s+)?SMD") ~ "SMD",
      TRUE ~ NA_character_
    ),
    
    # first signed number after the first "=" (handles the unicode minus
    # sign "\u2212" some Excel/Word exports use instead of a plain hyphen)
    est_val = {
      m <- str_extract(Estimate, "=\\s*[-\u2212]?\\s*[0-9]+\\.?[0-9]*")
      m <- str_remove(m, "=")
      m <- str_trim(m)
      m <- str_replace(m, "\u2212", "-")
      m <- str_remove_all(m, " ")
      suppressWarnings(as.numeric(m))
    },
    
    # first "95% CI (lo, hi)" pair in the string
    ci_low_raw  = str_match(Estimate, "CI\\s*\\(\\s*([-\u2212]?\\s*[0-9.]+)\\s*,")[, 2],
    ci_high_raw = str_match(Estimate, ",\\s*([-\u2212]?\\s*[0-9.]+)\\s*\\)")[, 2],
    ci_low  = suppressWarnings(as.numeric(str_remove_all(str_replace(ci_low_raw,  "\u2212", "-"), " "))),
    ci_high = suppressWarnings(as.numeric(str_remove_all(str_replace(ci_high_raw, "\u2212", "-"), " "))),
    
    # direction only -- no magnitude/conversion involved. SMD > 0 or OR < 1
    # both mean "favors the intervention" (confirmed against every row's
    # narrative interpretation in this file).
    direction = case_when(
      is.na(est_val) | is.na(metric) ~ NA_character_,
      metric == "SMD" & est_val > 0 ~ "Benefit",
      metric == "SMD" & est_val < 0 ~ "Harm",
      metric == "OR"  & est_val < 1 ~ "Benefit",
      metric == "OR"  & est_val > 1 ~ "Harm",
      TRUE ~ "No effect"  # est_val exactly at the null value
    ),
    
    # does the 95% CI exclude the null value? (1 for OR, 0 for SMD)
    significant = case_when(
      is.na(ci_low) | is.na(ci_high) | is.na(metric) ~ NA,
      metric == "OR"  ~ (ci_low - 1) * (ci_high - 1) > 0,
      metric == "SMD" ~ ci_low * ci_high > 0,
      TRUE ~ NA
    ),
    
    effect_cat = case_when(
      is.na(direction) ~ NA_character_,
      !is.na(significant) & !significant ~ "No effect",
      TRUE ~ direction
    ),
    
    certainty_clean = ifelse(Certainty %in% CERTAINTY_LEVELS, Certainty, NA_character_)
  ) %>%
  unite("row_key", all_of(ROW_GROUP_VARS), sep = " || ", remove = FALSE, na.rm = TRUE)

# Warn (don't silently hide) if a row/outcome combination has more than one
# estimate -- only the first would be shown per cell in that case.
dup_check <- prep %>% count(row_key, Outcome) %>% filter(n > 1)
if (nrow(dup_check) > 0) {
  warning(
    nrow(dup_check),
    " row/outcome combination(s) have more than one estimate; only the first ",
    "will be shown per cell. Add a column to ROW_GROUP_VARS if every ",
    "estimate needs to be shown separately."
  )
}

# outcome columns, alphabetical except "Helping Skills" pinned last
outcome_levels <- c(
  sort(setdiff(unique(prep$Outcome), "Helping Skills")),
  "Helping Skills"
)


# Build row labels ----
# One label per unique row_key: intervention name, time point, and (when
# present) a "Subgroup: ..." tag, rendered as HTML for the reactable cell.

row_meta <- prep %>%
  distinct(row_key, .keep_all = TRUE) %>%
  arrange(RefID, Intervention, Moderator, Time) %>%
  mutate(
    row_label_html = pmap_chr(
      list(RefID, Intervention, Time_clean, Moderator),
      function(refid, intervention, time_point, moderator) {
        
        heading <- if (SHOW_REFID_IN_LABEL) paste0(refid, ": ", intervention) else intervention
        
        label <- tags$div(
          tags$div(style = "font-weight:600;font-size:13px;line-height:1.35;", heading),
          tags$div(style = "font-size:11px;color:#868e96;margin-top:2px;", time_point),
          if (is_real_subgroup(moderator)) {
            tags$div(
              style = glue("font-size:11px;color:{COLORS$subgroup};margin-top:2px;"),
              paste0("Subgroup: ", moderator)
            )
          }
        )
        
        paste(as.character(label), collapse = "")
      }
    )
  ) %>%
  select(row_key, row_label_html)


# Build wide gap-map table ----
# Long (row_key x Outcome) -> wide (row_key x one column per Outcome),
# with each cell holding a CELL_SEP-joined payload the reactable cell
# renderer parses back apart.

cell_payload <- prep %>%
  distinct(row_key, Outcome, .keep_all = TRUE) %>%
  transmute(
    row_key, Outcome,
    payload = paste(row_key, Outcome, effect_cat, certainty_clean, na_blank(Estimate), sep = CELL_SEP)
  )

wide <- cell_payload %>%
  pivot_wider(names_from = Outcome, values_from = payload) %>%
  select(row_key, all_of(outcome_levels))

table_df <- row_meta %>%
  left_join(wide, by = "row_key") %>%
  select(row_key, row_label_html, all_of(outcome_levels))


# Reactable cell + column builders ----

# Pill-badge CSS for a GRADE certainty level -- shared by the reactable
# cells and the legend so the two always stay visually in sync.
certainty_badge_style <- function(level) {
  if (is.na(level) || !level %in% CERTAINTY_LEVELS) return("display:none;")
  
  color  <- CERTAINTY_COLORS[[level]]
  filled <- isTRUE(CERTAINTY_FILLED[[level]])
  
  glue(
    "font-size:11px;font-weight:600;padding:1px 8px;border-radius:10px;",
    "border:1px solid {color};",
    "color:{if (filled) '#fff' else color};",
    "background:{if (filled) color else '#fff'};"
  )
}

make_outcome_coldef <- function(outcome_name) {
  colDef(
    name = outcome_name,
    html = TRUE,
    align = "center",
    minWidth = 175,
    cell = function(value, index) {
      
      if (is.null(value) || is.na(value) || value == "") {
        return(tags$span("\u2014", style = "color:#adb5bd;"))
      }
      
      parts     <- strsplit(value, CELL_SEP, fixed = TRUE)[[1]]
      row_key   <- parts[1]
      outcome   <- parts[2]
      effect    <- parts[3]
      certainty <- parts[4]
      estimate  <- parts[5]
      
      # single-bracket `[` indexing returns NA (not an error) for an NA or
      # unmatched name, so `coalesce()` can safely supply the fallback
      effect_color <- coalesce(unname(EFFECT_COLORS[effect]), COLORS$text_light)
      cert_color   <- unname(CERTAINTY_COLORS[certainty])
      
      effect_display    <- if (is.na(effect)) "Not estimated" else effect
      certainty_display <- if (is.na(certainty)) "Not rated" else certainty
      cert_color_display <- if (is.na(cert_color)) "#000000" else cert_color
      
      cell_id <- paste(row_key, outcome, sep = CELL_SEP)
      onclick_js <- sprintf(
        "Shiny.setInputValue('cell_click', '%s', {priority: 'event'})",
        str_replace_all(cell_id, "'", "\\\\'")
      )
      
      # data-* attributes feed the floating hover tooltip (see app_js below)
      tags$div(
        onclick = onclick_js,
        class = "effect-cell",
        `data-effect` = effect_display,
        `data-effect-color` = effect_color,
        `data-estimate` = estimate,
        `data-certainty` = certainty_display,
        `data-certainty-color` = cert_color_display,
        style = "cursor:pointer;padding:6px 4px;",
        
        tags$div(
          style = "display:flex;flex-direction:column;align-items:center;gap:3px;",
          
          # effect indicator (color bar)
          tags$div(
            style = glue("display:flex;align-items:center;justify-content:center;gap:6px;font-size:12px;color:{COLORS$text_light};"),
            tags$span("Effect:"),
            tags$div(style = sprintf("width:65px;height:9px;border-radius:4px;background:%s;", effect_color))
          ),
          
          # certainty indicator (badge)
          tags$div(
            style = glue("display:flex;align-items:center;justify-content:center;gap:6px;font-size:12px;color:{COLORS$text_light};"),
            tags$span("Certainty:"),
            if (!is.na(certainty)) tags$span(certainty, style = certainty_badge_style(certainty))
          )
        )
      )
    }
  )
}

table_columns <- c(
  list(
    row_key = colDef(show = FALSE),
    row_label_html = colDef(
      name = "Intervention",
      html = TRUE,
      minWidth = 280,
      sticky = "left"
    )
  ),
  setNames(lapply(outcome_levels, make_outcome_coldef), outcome_levels)
)


# Legend builders ----

legend_effect_row <- function(label) {
  tags$div(
    style = "display:flex;align-items:center;gap:8px;margin-bottom:4px;",
    tags$div(style = sprintf("width:28px;height:10px;border-radius:4px;background:%s;", EFFECT_COLORS[[label]])),
    tags$span(label, style = "font-size:12px;")
  )
}

legend_certainty_row <- function(level) {
  tags$div(
    style = "margin-bottom:4px;",
    tags$span(level, style = certainty_badge_style(level))
  )
}


# UI ----

# Head CSS uses {{token}} (double-brace) delimiters so glue() doesn't
# collide with CSS's own single-brace rule blocks.
app_css <- glue(
  r"(
    body { font-family: -apple-system, 'Segoe UI', Roboto, sans-serif; }

    .app-title { font-size: 20px; font-weight: 700; margin: 14px 0 4px 0; }
    .app-subtitle { color: {{COLORS$text_light}}; margin-bottom: 16px; }

    .legend-box {
      border: 1px solid #e9ecef; border-radius: 8px; padding: 14px 16px;
      background: #fafbfc; margin-bottom: 16px;
    }
    .legend-heading {
      font-weight: 700; font-size: 13px; margin-bottom: 8px;
      text-transform: uppercase; letter-spacing: .03em; color: {{COLORS$text_muted}};
    }
    .legend-note { font-size: 11px; color: #868e96; margin-top: 8px; }
  
      .app-footer {
      font-size: 11px;
      color: #868e96;
      text-align: center;
      margin: 24px 0 12px 0;
    }
    .app-footer a {
      color: {{COLORS$subgroup}};
      text-decoration: none;
    }
    .app-footer a:hover {
      text-decoration: underline;
    }

    .table-scroll { overflow-x: auto; border: 1px solid #e9ecef; border-radius: 8px; }

    #detail_sidebar {
      position: fixed; top: 0; right: 0; width: 420px; height: 100vh;
      background: #fff; box-shadow: -4px 0 16px rgba(0,0,0,.12);
      padding: 20px; overflow-y: auto; z-index: 1000;
    }
    .detail-field { margin-bottom: 14px; }
    .detail-label {
      font-size: 11px; text-transform: uppercase; letter-spacing: .03em;
      color: {{COLORS$text_muted}}; font-weight: 700; margin-bottom: 2px;
    }
    .detail-value { font-size: 14px; color: {{COLORS$text}}; }

    .effect-cell { cursor: pointer; }

    #floating_tooltip {
      display: none; position: fixed; z-index: 900; width: 270px;
      padding: 14px 16px; background: #ffffff; border: 1px solid #e1e5ea;
      border-radius: 12px; box-shadow: 0 6px 20px rgba(0,0,0,.16);
      text-align: left; color: {{COLORS$text}}; pointer-events: none;
    }
    .tooltip-row {
      display: grid; grid-template-columns: 85px 1fr; gap: 8px;
      margin-bottom: 8px; align-items: center;
    }
    .tooltip-label { font-size: 11px; font-weight: 700; color: {{COLORS$text_light}}; letter-spacing: .04em; }
    .tooltip-value { font-size: 13px; font-weight: 600; color: {{COLORS$text}}; }
    #floating_tooltip hr { border: 0; border-top: 1px solid #e9ecef; margin: 10px 0; }
    .tooltip-hint { font-size: 12px; color: {{COLORS$text_muted}}; font-weight: 500; }
  )",
  .open = "{{", .close = "}}"
)

# Floating hover tooltip: reads the data-* attributes set on each
# .effect-cell (see make_outcome_coldef above) and positions itself above
# (or below, if there's no room) the hovered cell.
app_js <- r"(
  $(document).on('mouseenter', '.effect-cell', function() {
    var cell = $(this);
    var tooltip = $('#floating_tooltip');

    $('#tooltip_effect').text(cell.attr('data-effect')).css('color', cell.attr('data-effect-color'));
    $('#tooltip_estimate').text(cell.attr('data-estimate')).css('color', '#000000');
    $('#tooltip_certainty').text(cell.attr('data-certainty')).css('color', cell.attr('data-certainty-color'));

    var rect = this.getBoundingClientRect();
    tooltip.css({ display: 'block', visibility: 'hidden' });

    var tooltipWidth  = tooltip.outerWidth();
    var tooltipHeight = tooltip.outerHeight();

    var left = rect.left + (rect.width / 2) - (tooltipWidth / 2);
    var top  = rect.top - tooltipHeight - 10;

    left = Math.max(10, left);
    left = Math.min(left, window.innerWidth - tooltipWidth - 10);
    if (top < 10) top = rect.bottom + 10;  // not enough room above -> show below

    tooltip.css({ left: left + 'px', top: top + 'px', visibility: 'visible' });
  });

  $(document).on('mouseleave', '.effect-cell', function() { $('#floating_tooltip').hide(); });
  $(document).on('click', '.effect-cell', function() { $('#floating_tooltip').hide(); });
  $(window).on('scroll resize', function() { $('#floating_tooltip').hide(); });
)"

tooltip_row <- function(label, value_id) {
  tags$div(
    class = "tooltip-row",
    tags$span(class = "tooltip-label", label),
    tags$span(id = value_id, class = "tooltip-value")
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML(app_css)),
    tags$script(HTML(app_js))
  ),
  
  # floating hover tooltip shell (filled in by app_js on mouseenter)
  tags$div(
    id = "floating_tooltip",
    tooltip_row("EFFECT", "tooltip_effect"),
    tooltip_row("ESTIMATE", "tooltip_estimate"),
    tooltip_row("CERTAINTY", "tooltip_certainty"),
    tags$hr(),
    tags$div(class = "tooltip-hint", "Click within the cell for more details")
  ),
  
  div(class = "app-title", "Suicide Prevention: Overview of Reviews"),
  div(class = "app-subtitle", "Intervention benefits/harms by outcome domain, with GRADE certainty of evidence."),
  
  div(
    class = "legend-box",
    fluidRow(
      column(
        6,
        div(class = "legend-heading", "Effect vs comparator"),
        lapply(EFFECT_LABELS, legend_effect_row)
      ),
      column(
        6,
        div(class = "legend-heading", "Certainty of evidence (GRADE)"),
        lapply(CERTAINTY_LEVELS, legend_certainty_row),
        div(
          class = "legend-note",
          "Hover a cell for a quick summary. Click a cell for full details. ",
          "Rows tagged \"Subgroup: ...\" are moderator/subgroup comparisons, ",
          "not a review's headline finding -- read the narrative for those."
        )
      )
    )
  ),
  
  div(class = "table-scroll", reactableOutput("gapmap_table")),
  
  conditionalPanel(
    condition = "output.sidebar_visible",
    div(
      id = "detail_sidebar",
      actionButton("close_sidebar", "\u2715", style = "float:right;border:none;background:none;font-size:16px;"),
      uiOutput("detail_panel")
    )
  ),
  div(
    class = "app-footer",
    "Dashboard layout inspired by the ",
    tags$a(
      href = "https://u-reach.org/",
      target = "_blank",
      rel = "noopener noreferrer",
      "U-REACH"
    ),
    " evidence platforms for overview findings."
  )
)


# Server ----

server <- function(input, output, session) {
  
  selected_detail <- reactiveVal(NULL)
  
  output$gapmap_table <- renderReactable({
    reactable(
      table_df,
      columns = table_columns,
      searchable = TRUE,
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      wrap = FALSE,
      defaultPageSize = 25,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 25, 50, 100)
    )
  })
  
  observeEvent(input$cell_click, {
    parts <- strsplit(input$cell_click, CELL_SEP, fixed = TRUE)[[1]]
    rk <- parts[1]
    oc <- parts[2]
    detail <- prep %>% filter(row_key == rk, Outcome == oc) %>% slice(1)
    if (nrow(detail) == 1) selected_detail(detail)
  })
  
  observeEvent(input$close_sidebar, {
    selected_detail(NULL)
  })
  
  output$sidebar_visible <- reactive(!is.null(selected_detail()))
  outputOptions(output, "sidebar_visible", suspendWhenHidden = FALSE)
  
  detail_field <- function(label, value) {
    value <- as.character(value)
    if (is.na(value) || value == "") value <- "\u2014"
    div(
      class = "detail-field",
      div(class = "detail-label", label),
      div(class = "detail-value", value)
    )
  }
  
  output$detail_panel <- renderUI({
    d <- selected_detail()
    req(d)
    
    tagList(
      h4(d$Intervention),
      h5(d$Outcome, style = "color:#4D5859;font-weight:400;margin-bottom:18px;"),
      detail_field("Population", d$Population),
      detail_field("Time point", d$Time_clean),
      if (is_real_subgroup(d$Moderator)) detail_field("Subgroup / moderator comparison", d$Moderator),
      detail_field("Comparator(s)", d$Comparator_clean),
      detail_field("Estimate", d$Estimate),
      detail_field("Certainty (GRADE)", d$Certainty),
      hr(),
      div(class = "legend-heading", "GRADE domains"),
      detail_field("Study limitations", d$`Study Limitations`),
      detail_field("Inconsistency", d$Inconsistency),
      detail_field("Indirectness", d$Indirectness),
      detail_field("Imprecision", d$Imprecision),
      detail_field("Publication bias", d$`Publication Bias`),
      hr(),
      detail_field("Narrative interpretation", d$`Narrative Interpretation`),
      detail_field("Applicability concerns", d$`Applicability concerns`),
      if (not_blank(d$pdf_link)) {
        div(
          class = "detail-field",
          div(class = "detail-label", "Review Source"),
          tags$a(href = d$pdf_link, target = "_blank", rel = "noopener noreferrer", paste0(d$review_name, " \u2197"))
        )
      },
      detail_field("Reference ID", d$RefID),
      detail_field("Estimate ID", d$EstimateID)
    )
  })
}


# Run app ----

shinyApp(ui, server)