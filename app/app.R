# ─────────────────────────────────────────────────────────────────────────────
# app.R  —  Study Participant Dashboard v3 (Shiny)
# MOCK-STUDY-001
#
# Required packages:
#   install.packages(c("shiny", "shinydashboard", "DT", "dplyr",
#                      "readr", "lubridate", "shinyWidgets", "htmltools"))
#
# Run with:  shiny::runApp("app.R")
# ─────────────────────────────────────────────────────────────────────────────

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(readr)
library(lubridate)
library(shinyWidgets)
library(htmltools)

# ── Null-coalescing helper (replaces %||% which requires R >= 4.4) ────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1]) && a[1] != "") a else b

safe_val <- function(x, fallback = "—") {
  if (is.null(x) || length(x) == 0 || is.na(x) || x == "") fallback else as.character(x)
}

# ── Site metadata ─────────────────────────────────────────────────────────────
SITE_META <- list(
  "SITE-001" = list(name="Boston Medical Center",      city="Boston",      state="MA", pi="Dr. Harmon",  coord="Lisa Tran",    color="#185fa5"),
  "SITE-002" = list(name="UCLA Health Sciences",       city="Los Angeles", state="CA", pi="Dr. Okafor",  coord="James Wu",     color="#639922"),
  "SITE-003" = list(name="UT Southwestern Medical",    city="Dallas",      state="TX", pi="Dr. Patel",   coord="Maria Santos", color="#ba7517"),
  "SITE-004" = list(name="Johns Hopkins Research Ctr", city="Baltimore",   state="MD", pi="Dr. Müller",  coord="Aisha Brown",  color="#a32d2d"),
  "SITE-005" = list(name="University of Chicago Med",  city="Chicago",     state="IL", pi="Dr. Rivera",  coord="Tom Nguyen",   color="#5b2d8e")
)

site_color <- function(code) {
  m <- SITE_META[[code]]
  if (is.null(m)) "#888" else m$color
}

# ── Helpers ───────────────────────────────────────────────────────────────────
priority_of <- function(status) {
  dplyr::case_when(
    status == "Screen Failure" ~ 1L,
    status == "Dropped Out"   ~ 2L,
    status == "Completed"     ~ 3L,
    TRUE                      ~ 4L
  )
}

status_color <- function(status) {
  switch(status,
    "Screen Failure" = "#a32d2d",
    "Dropped Out"    = "#854f0b",
    "Completed"      = "#3b6d11",
    "Active"         = "#0c447c",
    "#5f5e5a"
  )
}

status_bg <- function(status) {
  switch(status,
    "Screen Failure" = "#fcebeb",
    "Dropped Out"    = "#faeeda",
    "Completed"      = "#eaf3de",
    "Active"         = "#e6f1fb",
    "#f1efe8"
  )
}

review_color <- function(rv) {
  switch(rv,
    "Complete"    = "#3b6d11",
    "In Progress" = "#5b2d8e",
    "#5f5e5a"
  )
}

review_bg <- function(rv) {
  switch(rv,
    "Complete"    = "#eaf3de",
    "In Progress" = "#f0eafa",
    "#f1efe8"
  )
}

pill_html <- function(label, bg, color) {
  sprintf(
    '<span style="display:inline-block;font-size:11px;font-weight:600;padding:2px 8px;border-radius:20px;font-family:Courier New,monospace;background:%s;color:%s">%s</span>',
    bg, color, label
  )
}

visit_bar_html <- function(n, total = 5) {
  pct <- round((as.integer(n) / total) * 100)
  sprintf(
    '<div style="font-size:12px;color:#5f5e5a">%d/%d</div><div style="height:5px;background:#e0ddd6;border-radius:3px;min-width:60px;overflow:hidden"><div style="height:100%%;width:%d%%;background:#639922;border-radius:3px"></div></div>',
    as.integer(n), total, pct
  )
}

fmt_date <- function(d) {
  if (is.null(d) || length(d) == 0 || is.na(d) || d == "" || d == "\u2014") return("\u2014")
  tryCatch(format(as.Date(d), "%b %d, %Y"), error = function(e) as.character(d))
}

drow <- function(key, val) {
  div(
    style = "display:flex;justify-content:space-between;padding:3px 0;font-size:12.5px;border-bottom:0.5px solid rgba(0,0,0,0.06);",
    span(style = "color:#5f5e5a;", key),
    span(style = "font-weight:500;color:#1a1a18;text-align:right;", val)
  )
}

# ── Data builder ──────────────────────────────────────────────────────────────
build_participants <- function(subjects, visits = NULL, labs = NULL,
                               adverse = NULL, screening = NULL) {
  p <- subjects %>%
    rename_with(tolower) %>%
    mutate(across(everything(), as.character))

  # Visits
  if (!is.null(visits) && nrow(visits) > 0) {
    v <- visits %>% rename_with(tolower) %>% mutate(across(everything(), as.character))
    vsumm <- v %>%
      group_by(subject_id) %>%
      summarise(
        visits_completed = sum(status == "Completed", na.rm = TRUE),
        visits_missed    = sum(status == "Missed",    na.rm = TRUE),
        last_sched_date  = {
          dates <- suppressWarnings(as.Date(scheduled_date[scheduled_date != ""]))
          dates <- dates[!is.na(dates)]
          if (length(dates) == 0) NA_character_ else as.character(max(dates))
        },
        .groups = "drop"
      )
    p <- left_join(p, vsumm, by = "subject_id")
  } else {
    p <- p %>% mutate(
      visits_completed = ifelse(completed_study == "Yes", 5L, 0L),
      visits_missed    = 0L,
      last_sched_date  = NA_character_
    )
  }

  # Labs
  if (!is.null(labs) && nrow(labs) > 0) {
    l <- labs %>% rename_with(tolower) %>% mutate(across(everything(), as.character))
    lsumm <- l %>%
      group_by(subject_id) %>%
      summarise(
        lab_normal = sum(flag == "Normal", na.rm = TRUE),
        lab_high   = sum(flag == "HIGH",   na.rm = TRUE),
        lab_low    = sum(flag == "LOW",    na.rm = TRUE),
        .groups = "drop"
      )
    p <- left_join(p, lsumm, by = "subject_id")
  } else {
    p <- p %>% mutate(lab_normal = 0L, lab_high = 0L, lab_low = 0L)
  }

  # Adverse events
  if (!is.null(adverse) && nrow(adverse) > 0) {
    a <- adverse %>% rename_with(tolower) %>% mutate(across(everything(), as.character))
    asumm <- a %>%
      group_by(subject_id) %>%
      summarise(
        ae_count  = n(),
        ae_severe = sum(severity == "Severe", na.rm = TRUE),
        ae_list   = paste(adverse_event, collapse = "; "),
        ae_detail = paste(sprintf("%s (%s)", adverse_event, severity), collapse = "\n"),
        .groups = "drop"
      )
    p <- left_join(p, asumm, by = "subject_id")
  } else {
    p <- p %>% mutate(ae_count = 0L, ae_severe = 0L, ae_list = "None", ae_detail = "None reported")
  }

  # Screening
  if (!is.null(screening) && nrow(screening) > 0) {
    s <- screening %>%
      rename_with(tolower) %>%
      select(subject_id, any_of(c("overall_eligible", "exclusion_condition",
                                   "consent_date", "consent_version", "referring_source"))) %>%
      mutate(across(everything(), as.character))
    p <- left_join(p, s, by = "subject_id", suffix = c("", ".scr"))
  }

  # Ensure columns exist
  for (col in c("overall_eligible","exclusion_condition","consent_date",
                 "consent_version","referring_source","site_code","site_name")) {
    if (!col %in% names(p)) p[[col]] <- NA_character_
  }

  p <- p %>%
    mutate(
      visits_completed    = as.integer(tidyr::replace_na(visits_completed, 0)),
      visits_missed       = as.integer(tidyr::replace_na(visits_missed, 0)),
      lab_normal          = as.integer(tidyr::replace_na(lab_normal, 0)),
      lab_high            = as.integer(tidyr::replace_na(lab_high,   0)),
      lab_low             = as.integer(tidyr::replace_na(lab_low,    0)),
      ae_count            = as.integer(tidyr::replace_na(ae_count,   0)),
      ae_severe           = as.integer(tidyr::replace_na(ae_severe,  0)),
      overall_eligible    = tidyr::replace_na(overall_eligible,    "Yes"),
      exclusion_condition = tidyr::replace_na(exclusion_condition, "None"),
      ae_list             = tidyr::replace_na(ae_list,    "None"),
      ae_detail           = tidyr::replace_na(ae_detail,  "None reported"),
      consent_date        = tidyr::replace_na(consent_date,        "\u2014"),
      consent_version     = tidyr::replace_na(consent_version,     "\u2014"),
      referring_source    = tidyr::replace_na(referring_source,    "\u2014"),
      site_code           = tidyr::replace_na(site_code, ""),
      site_name           = tidyr::replace_na(site_name, ""),

      study_status = dplyr::case_when(
        overall_eligible == "No"                                           ~ "Screen Failure",
        completed_study  == "No" & visits_completed < 5 & visits_missed > 0 ~ "Dropped Out",
        completed_study  == "Yes"                                          ~ "Completed",
        TRUE                                                                ~ "Active"
      ),
      priority = priority_of(study_status),

      expected_completion = dplyr::case_when(
        study_status == "Active" & !is.na(last_sched_date) ~ last_sched_date,
        TRUE ~ NA_character_
      ),

      lab_total    = lab_normal + lab_high + lab_low,
      pct_abnormal = ifelse(lab_total > 0, round((lab_high + lab_low) / lab_total * 100), 0L),

      exclusion_reason = ifelse(
        exclusion_condition %in% c("None", "", NA), "\u2014", exclusion_condition
      )
    )

  p
}

# ── CSS ───────────────────────────────────────────────────────────────────────
custom_css <- "
  body, .content-wrapper, .main-sidebar, .wrapper { background:#f5f4f0 !important; font-family:Georgia,serif; }
  .skin-blue .main-header .logo, .skin-blue .main-header .navbar { background:#ffffff !important; border-bottom:1px solid rgba(0,0,0,0.1); }
  .skin-blue .main-header .logo { color:#1a1a18 !important; font-size:18px; font-weight:600; }
  .skin-blue .main-sidebar { background:#ffffff !important; border-right:1px solid rgba(0,0,0,0.1); }
  .skin-blue .sidebar-menu > li > a { color:#5f5e5a !important; font-size:13px; font-family:'Courier New',monospace; }
  .skin-blue .sidebar-menu > li.active > a,
  .skin-blue .sidebar-menu > li > a:hover { background:#e6f1fb !important; color:#0c447c !important; }
  .skin-blue .sidebar-menu > li.active > a { border-left:3px solid #185fa5 !important; }
  .box { border-radius:10px !important; box-shadow:none !important; border:0.5px solid rgba(0,0,0,0.09) !important; }
  .box-header { border-bottom:0.5px solid rgba(0,0,0,0.09) !important; padding:12px 16px !important; }
  .box-title { font-size:13px !important; font-family:'Courier New',monospace !important; font-weight:600 !important; text-transform:uppercase; letter-spacing:0.6px; color:#999891 !important; }
  .stat-box { background:#ffffff; border:0.5px solid rgba(0,0,0,0.09); border-radius:10px; padding:14px 16px; margin-bottom:10px; }
  .stat-label { font-size:11px; color:#999891; text-transform:uppercase; letter-spacing:0.8px; font-family:'Courier New',monospace; margin-bottom:6px; }
  .stat-value { font-size:26px; font-weight:600; line-height:1; }
  .stat-sub   { font-size:11px; color:#999891; font-family:'Courier New',monospace; margin-top:4px; }
  .site-card { background:#ffffff; border:0.5px solid rgba(0,0,0,0.09); border-radius:10px; padding:16px; margin-bottom:12px; }
  .priority-header { font-size:13px; font-weight:600; color:#5f5e5a; text-transform:uppercase; letter-spacing:0.6px; padding:10px 0 8px; border-bottom:0.5px solid rgba(0,0,0,0.09); margin-bottom:12px; display:flex; align-items:center; gap:10px; }
  .p-badge { font-size:11px; font-family:'Courier New',monospace; font-weight:600; padding:3px 9px; border-radius:20px; }
  .form-control { border-radius:6px !important; border:0.5px solid rgba(0,0,0,0.16) !important; font-size:13px !important; }
  .form-control:focus { border-color:#185fa5 !important; box-shadow:none !important; }
  .btn-export { background:#eaf3de; color:#3b6d11; border:0.5px solid #639922; border-radius:6px; font-size:12px; font-family:'Courier New',monospace; font-weight:600; padding:6px 14px; }
  .nav-tabs > li > a { font-family:'Courier New',monospace; font-size:13px; font-weight:600; color:#999891; }
  .nav-tabs > li.active > a { color:#0c447c !important; border-bottom:2px solid #185fa5 !important; }
  .dataTables_wrapper { font-size:13px; }
  table.dataTable thead th { font-size:11px !important; font-family:'Courier New',monospace !important; text-transform:uppercase; letter-spacing:0.6px; color:#999891 !important; background:#f1efe8 !important; font-weight:600 !important; }
  table.dataTable tbody tr { cursor:pointer; }
  table.dataTable tbody tr:hover td { background:#f1efe8 !important; }
  .review-box { background:#ffffff; border:1px solid #8b5cf6; border-radius:10px; padding:16px; margin-top:12px; }
  .review-box-title { font-size:11px; font-family:'Courier New',monospace; font-weight:600; text-transform:uppercase; letter-spacing:0.6px; color:#5b2d8e; margin-bottom:12px; }
  .detail-section { margin-bottom:14px; }
  .detail-section-title { font-size:11px; font-family:'Courier New',monospace; color:#999891; text-transform:uppercase; letter-spacing:0.6px; font-weight:600; margin-bottom:6px; padding-bottom:4px; border-bottom:0.5px solid rgba(0,0,0,0.09); }
  .well { background:#ffffff !important; border:0.5px solid rgba(0,0,0,0.09) !important; border-radius:10px !important; box-shadow:none !important; }
"

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Study Dashboard v3"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Files",        tabName = "upload",       icon = icon("upload")),
      menuItem("Participants",        tabName = "participants", icon = icon("users")),
      menuItem("Site Visit Schedule", tabName = "schedule",     icon = icon("calendar")),
      menuItem("Export Review CSV",   tabName = "export",       icon = icon("download"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    tabItems(

      # ── Upload ──────────────────────────────────────────────────────────────
      tabItem(tabName = "upload",
        box(width = 12, title = "Load Study Files",
          p(style = "font-size:13px;color:#999891;font-family:'Courier New',monospace;margin-bottom:1.5rem;",
            "MOCK-STUDY-001 — Upload your CSV files. Only mock_study_data_v2.csv is required."),
          fluidRow(
            column(6,
              fileInput("file_subjects",  "mock_study_data_v2.csv (Required)",  accept = ".csv"),
              fileInput("file_visits",    "mock_visit_data.csv (Optional)",      accept = ".csv"),
              fileInput("file_labs",      "mock_lab_results.csv (Optional)",     accept = ".csv")
            ),
            column(6,
              fileInput("file_adverse",   "mock_adverse_events.csv (Optional)",      accept = ".csv"),
              fileInput("file_screening", "mock_consent_screening.csv (Optional)",   accept = ".csv"),
              fileInput("file_schedule",  "mock_site_visit_schedule.csv (Optional)", accept = ".csv"),
              fileInput("file_review",    "mock_review_status.csv (Resume Progress)", accept = ".csv")
            )
          ),
          uiOutput("upload_status")
        )
      ),

      # ── Participants ─────────────────────────────────────────────────────────
      tabItem(tabName = "participants",
        uiOutput("stats_row"),
        uiOutput("site_cards_ui"),
        box(width = 12, title = "Filters",
          fluidRow(
            column(3, textInput("search", NULL, placeholder = "Search ID, condition, site\u2026")),
            column(2, selectInput("filter_site",      NULL, choices = c("All sites" = ""))),
            column(2, selectInput("filter_condition", NULL, choices = c("All conditions" = ""))),
            column(3, selectInput("filter_priority",  NULL,
              choices = c("All study priorities" = "",
                          "P1 \u2014 Screen Failures" = "1",
                          "P2 \u2014 Dropped Out"     = "2",
                          "P3 \u2014 Completed"       = "3",
                          "P4 \u2014 Active"          = "4"))),
            column(2, selectInput("filter_review", NULL,
              choices = c("All review statuses" = "",
                          "Not Started" = "Not Started",
                          "In Progress" = "In Progress",
                          "Complete"    = "Complete")))
          )
        ),
        uiOutput("participant_tables")
      ),

      # ── Schedule ─────────────────────────────────────────────────────────────
      tabItem(tabName = "schedule",
        box(width = 12, title = "Filters",
          fluidRow(
            column(3, selectInput("sched_site",   NULL, choices = c("All sites" = ""))),
            column(3, selectInput("sched_type",   NULL,
              choices = c("All visit types" = "",
                          "Participant Visit" = "Participant Visit",
                          "Monitoring Visit"  = "Monitoring Visit"))),
            column(3, selectInput("sched_status", NULL,
              choices = c("All statuses" = "",
                          "Scheduled" = "Scheduled", "Completed" = "Completed",
                          "Rescheduled" = "Rescheduled", "Not Reached" = "Not Reached",
                          "Cancelled" = "Cancelled"))),
            column(3, selectInput("sched_window", NULL,
              choices = c("All dates" = "",
                          "Past" = "past", "Next 30 days" = "30",
                          "Next 60 days" = "60", "Next 90 days" = "90")))
          )
        ),
        uiOutput("schedule_ui")
      ),

      # ── Export ───────────────────────────────────────────────────────────────
      tabItem(tabName = "export",
        box(width = 6, title = "Export Review Status CSV",
          p(style = "font-size:13px;color:#5f5e5a;margin-bottom:1rem;",
            "Download the current review status for all participants. Re-upload on the Upload tab to resume progress."),
          downloadButton("download_review", "\u2b07 Download mock_review_status.csv", class = "btn-export")
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Raw CSV reactives ──────────────────────────────────────────────────────
  subjects_raw  <- reactive({ req(input$file_subjects);  read_csv(input$file_subjects$datapath,  show_col_types = FALSE) })
  visits_raw    <- reactive({ if (is.null(input$file_visits))    return(NULL); read_csv(input$file_visits$datapath,    show_col_types = FALSE) })
  labs_raw      <- reactive({ if (is.null(input$file_labs))      return(NULL); read_csv(input$file_labs$datapath,      show_col_types = FALSE) })
  adverse_raw   <- reactive({ if (is.null(input$file_adverse))   return(NULL); read_csv(input$file_adverse$datapath,   show_col_types = FALSE) })
  screening_raw <- reactive({ if (is.null(input$file_screening)) return(NULL); read_csv(input$file_screening$datapath, show_col_types = FALSE) })
  schedule_raw  <- reactive({
    if (is.null(input$file_schedule)) return(NULL)
    read_csv(input$file_schedule$datapath, show_col_types = FALSE) %>%
      rename_with(tolower) %>%
      mutate(across(everything(), as.character))
  })

  # ── Review state ───────────────────────────────────────────────────────────
  review_state <- reactiveVal(
    data.frame(subject_id=character(), review_status=character(),
               review_notes=character(), reviewer=character(),
               last_updated=character(), stringsAsFactors=FALSE)
  )

  observeEvent(input$file_review, {
    rv <- read_csv(input$file_review$datapath, show_col_types = FALSE) %>%
      rename_with(tolower) %>%
      mutate(across(everything(), as.character))
    if ("subject_id" %in% names(rv)) {
      merged <- bind_rows(review_state(), rv) %>%
        group_by(subject_id) %>% slice_tail(n = 1) %>% ungroup()
      review_state(as.data.frame(merged))
    }
  })

  get_review <- function(sid) {
    rv <- review_state() %>% filter(subject_id == sid)
    if (nrow(rv) == 0)
      return(list(status = "Not Started", notes = "", reviewer = "", timestamp = ""))
    list(
      status    = if (is.na(rv$review_status[1])  || rv$review_status[1]  == "") "Not Started" else rv$review_status[1],
      notes     = if (is.na(rv$review_notes[1])   || rv$review_notes[1]   == "") "" else rv$review_notes[1],
      reviewer  = if (is.na(rv$reviewer[1])        || rv$reviewer[1]        == "") "" else rv$reviewer[1],
      timestamp = if (is.na(rv$last_updated[1])    || rv$last_updated[1]    == "") "" else rv$last_updated[1]
    )
  }

  save_review <- function(sid, status, notes) {
    new_row <- data.frame(
      subject_id    = sid,
      review_status = status,
      review_notes  = notes,
      reviewer      = "User",
      last_updated  = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      stringsAsFactors = FALSE
    )
    updated <- bind_rows(review_state() %>% filter(subject_id != sid), new_row)
    review_state(as.data.frame(updated))
  }

  # ── Participants reactive ──────────────────────────────────────────────────
  participants <- reactive({
    req(subjects_raw())
    build_participants(subjects_raw(), visits_raw(), labs_raw(), adverse_raw(), screening_raw())
  })

  # Populate filter dropdowns on load
  observeEvent(participants(), {
    p <- participants()
    sites <- sort(unique(p$site_code[p$site_code != "" & !is.na(p$site_code)]))
    site_labels <- sapply(sites, function(s) {
      m <- SITE_META[[s]]
      if (is.null(m)) s else paste0(s, " \u2014 ", m$name)
    })
    site_choices <- setNames(sites, site_labels)
    all_sites <- c("All sites" = "", site_choices)
    updateSelectInput(session, "filter_site", choices = all_sites)
    updateSelectInput(session, "sched_site",  choices = all_sites)

    conds <- sort(unique(p$study_condition[!is.na(p$study_condition) & p$study_condition != ""]))
    updateSelectInput(session, "filter_condition",
                      choices = c("All conditions" = "", setNames(conds, conds)))
  })

  # ── Filtered data ──────────────────────────────────────────────────────────
  filtered_participants <- reactive({
    req(participants())
    p  <- participants()
    rv <- review_state()

    # Search
    srch <- tolower(trimws(input$search))
    if (nchar(srch) > 0) {
      haystack <- tolower(paste(p$subject_id, p$study_condition, p$study_status, p$site_code))
      p <- p[grepl(srch, haystack, fixed = TRUE), ]
    }

    # Dropdowns
    if (!is.null(input$filter_site)      && input$filter_site      != "") p <- p %>% filter(site_code      == input$filter_site)
    if (!is.null(input$filter_condition) && input$filter_condition != "") p <- p %>% filter(study_condition == input$filter_condition)
    if (!is.null(input$filter_priority)  && input$filter_priority  != "") p <- p %>% filter(priority        == as.integer(input$filter_priority))

    if (!is.null(input$filter_review) && input$filter_review != "") {
      target <- input$filter_review
      sids_match <- rv %>% filter(review_status == target) %>% pull(subject_id)
      if (target == "Not Started") {
        p <- p %>% filter(!(subject_id %in% rv$subject_id) | subject_id %in% sids_match)
      } else {
        p <- p %>% filter(subject_id %in% sids_match)
      }
    }
    p
  })

  # ── Upload status ──────────────────────────────────────────────────────────
  output$upload_status <- renderUI({
    n_loaded <- sum(c(
      !is.null(input$file_subjects), !is.null(input$file_visits),
      !is.null(input$file_labs),     !is.null(input$file_adverse),
      !is.null(input$file_screening),!is.null(input$file_schedule),
      !is.null(input$file_review)
    ))
    ready_msg <- if (!is.null(input$file_subjects))
      p(style = "font-size:13px;color:#3b6d11;",
        sprintf("\u2713 %d participants ready.", nrow(participants())))
    else
      NULL
    tagList(hr(), p(style = "font-size:13px;color:#999891;font-family:'Courier New',monospace;",
                    sprintf("%d / 7 files loaded.", n_loaded)), ready_msg)
  })

  # ── Stat cards ─────────────────────────────────────────────────────────────
  output$stats_row <- renderUI({
    req(participants())
    p   <- filtered_participants()
    all <- participants()
    rv  <- review_state()
    reviewed <- nrow(rv %>% filter(review_status == "Complete"))
    in_prog  <- nrow(rv %>% filter(review_status == "In Progress"))

    mk_stat <- function(label, val, color = "#1a1a18", sub = NULL) {
      div(class = "stat-box",
        div(class = "stat-label", label),
        div(class = "stat-value", style = paste0("color:", color), val),
        if (!is.null(sub)) div(class = "stat-sub", sub) else NULL
      )
    }

    fluidRow(
      column(2, mk_stat("Total",           nrow(p))),
      column(2, mk_stat("Active",          sum(p$study_status == "Active"),         "#0c447c")),
      column(2, mk_stat("Completed",       sum(p$study_status == "Completed"),      "#3b6d11")),
      column(2, mk_stat("Dropped Out",     sum(p$study_status == "Dropped Out"),    "#854f0b")),
      column(2, mk_stat("Screen Failures", sum(p$study_status == "Screen Failure"), "#a32d2d")),
      column(2, mk_stat("Data Reviewed",
                         sprintf("%d/%d", reviewed, nrow(all)), "#5b2d8e",
                         sub = sprintf("%d in progress", in_prog)))
    )
  })

  # ── Site cards ─────────────────────────────────────────────────────────────
  output$site_cards_ui <- renderUI({
    req(participants())
    p     <- participants()
    sched <- schedule_raw()
    today <- Sys.Date()
    sites <- sort(unique(p$site_code[p$site_code != "" & !is.na(p$site_code)]))
    n     <- length(sites)
    if (n == 0) return(NULL)
    col_w <- max(2L, min(4L, as.integer(floor(12 / n))))

    cards <- lapply(sites, function(code) {
      meta <- SITE_META[[code]]
      if (is.null(meta)) meta <- list(name=code, city="", state="", pi="\u2014", coord="\u2014", color="#888")
      sp <- p %>% filter(site_code == code)

      next_visit <- "\u2014"
      if (!is.null(sched) && nrow(sched) > 0) {
        up <- sched %>%
          filter(site_code == code, status == "Scheduled") %>%
          mutate(sd = suppressWarnings(as.Date(scheduled_date))) %>%
          filter(!is.na(sd), sd >= today) %>%
          arrange(sd)
        if (nrow(up) > 0)
          next_visit <- sprintf("%s \u2014 %s",
                                format(up$sd[1], "%b %d, %Y"),
                                safe_val(up$visit_name[1], safe_val(up$visit_type[1], "")))
      }

      dropped_tag <- if (sum(sp$study_status == "Dropped Out") > 0)
        tags$span(style="display:inline-block;padding:2px 7px;border-radius:20px;font-size:11px;font-family:'Courier New',monospace;font-weight:600;background:#faeeda;color:#854f0b;margin-right:4px;",
                  sprintf("%d dropped", sum(sp$study_status == "Dropped Out")))
      else NULL

      failed_tag <- if (sum(sp$study_status == "Screen Failure") > 0)
        tags$span(style="display:inline-block;padding:2px 7px;border-radius:20px;font-size:11px;font-family:'Courier New',monospace;font-weight:600;background:#fcebeb;color:#a32d2d;margin-right:4px;",
                  sprintf("%d failed", sum(sp$study_status == "Screen Failure")))
      else NULL

      column(col_w,
        div(class = "site-card",
            style = sprintf("border-left:3px solid %s;", meta$color),
          div(style = sprintf("font-size:11px;font-family:'Courier New',monospace;font-weight:700;color:%s;letter-spacing:0.8px;margin-bottom:4px;", meta$color), code),
          div(style = "font-size:14px;font-weight:600;margin-bottom:2px;", meta$name),
          div(style = "font-size:12px;color:#999891;margin-bottom:8px;font-family:'Courier New',monospace;",
              sprintf("%s, %s", meta$city, meta$state)),
          div(style = "font-size:12px;color:#5f5e5a;margin-bottom:10px;",
              sprintf("PI: %s \u00b7 CRC: %s", meta$pi, meta$coord)),
          div(
            tags$span(style="display:inline-block;padding:2px 7px;border-radius:20px;font-size:11px;font-family:'Courier New',monospace;font-weight:600;background:#f1efe8;color:#5f5e5a;margin-right:4px;",
                      sprintf("%d total", nrow(sp))),
            tags$span(style="display:inline-block;padding:2px 7px;border-radius:20px;font-size:11px;font-family:'Courier New',monospace;font-weight:600;background:#e6f1fb;color:#0c447c;margin-right:4px;",
                      sprintf("%d active", sum(sp$study_status == "Active"))),
            tags$span(style="display:inline-block;padding:2px 7px;border-radius:20px;font-size:11px;font-family:'Courier New',monospace;font-weight:600;background:#eaf3de;color:#3b6d11;margin-right:4px;",
                      sprintf("%d done", sum(sp$study_status == "Completed"))),
            dropped_tag,
            failed_tag
          ),
          div(style = "font-size:11px;color:#0e6b6b;font-family:'Courier New',monospace;margin-top:8px;padding-top:8px;border-top:0.5px solid rgba(0,0,0,0.09);",
              sprintf("\U0001f4c5 Next: %s", next_visit))
        )
      )
    })
    do.call(fluidRow, cards)
  })

  # ── Participant tables ──────────────────────────────────────────────────────
  output$participant_tables <- renderUI({
    req(participants())
    groups <- list(
      list(priority=1L, label="Screen Failures",                badge_bg="#fcebeb", badge_col="#a32d2d"),
      list(priority=2L, label="Dropped Out",                    badge_bg="#faeeda", badge_col="#854f0b"),
      list(priority=3L, label="Completed",                      badge_bg="#eaf3de", badge_col="#3b6d11"),
      list(priority=4L, label="Active \u2014 by Expected Completion", badge_bg="#e6f1fb", badge_col="#0c447c")
    )
    p <- filtered_participants()
    ui_list <- lapply(groups, function(grp) {
      gp <- p %>% filter(priority == grp$priority)
      if (nrow(gp) == 0) return(NULL)
      table_id <- paste0("ptable_", grp$priority)
      box(width = 12,
        div(class = "priority-header",
          tags$span(class = "p-badge",
            style = sprintf("background:%s;color:%s;", grp$badge_bg, grp$badge_col),
            sprintf("P%d", grp$priority)),
          grp$label,
          tags$span(style = "font-size:12px;color:#999891;font-family:'Courier New',monospace;font-weight:400;",
            sprintf("%d participant%s", nrow(gp), ifelse(nrow(gp) != 1, "s", "")))
        ),
        DTOutput(table_id)
      )
    })
    tagList(ui_list)
  })

  # ── DT render helper ──────────────────────────────────────────────────────
  render_priority_table <- function(priority_num) {
    renderDT({
      req(participants())
      p <- filtered_participants() %>% filter(priority == priority_num)
      if (priority_num == 4L) p <- p %>% arrange(expected_completion)
      rv <- review_state()
      if (nrow(p) == 0) return(data.frame(Message = "No participants in this group."))

      p <- p %>%
        left_join(rv %>% select(subject_id, review_status), by = "subject_id") %>%
        mutate(review_status = tidyr::replace_na(review_status, "Not Started"))

      site_col <- mapply(function(code) {
        col <- site_color(code)
        sprintf(
          '<span style="display:inline-flex;align-items:center;gap:5px;"><span style="width:8px;height:8px;border-radius:50%%;background:%s;display:inline-block;"></span><span style="font-family:Courier New,monospace;color:%s;font-weight:600;font-size:11px;">%s</span></span>',
          col, col, code)
      }, p$site_code)

      status_col  <- sapply(p$study_status,  function(s) pill_html(s,   status_bg(s),  status_color(s)))
      review_col  <- sapply(p$review_status, function(rv) pill_html(rv, review_bg(rv), review_color(rv)))
      visit_col   <- sapply(p$visits_completed, visit_bar_html)
      ae_col      <- sapply(p$ae_count, function(n)
                      pill_html(as.character(n),
                                ifelse(n > 0, "#faeeda", "#f1efe8"),
                                ifelse(n > 0, "#854f0b", "#5f5e5a")))

      df <- data.frame(
        ` `          = rep("\u25b6", nrow(p)),
        ID           = p$subject_id,
        Site         = site_col,
        Age          = p$age,
        Condition    = p$study_condition,
        Status       = status_col,
        Visits       = visit_col,
        AEs          = ae_col,
        `Data Review`= review_col,
        check.names  = FALSE, stringsAsFactors = FALSE
      )
      if (priority_num == 1L) df[["Exclusion"]]       <- p$exclusion_reason
      if (priority_num == 4L) df[["Est. Completion"]] <- sapply(p$expected_completion, fmt_date)
      df
    },
    escape    = FALSE,
    selection = "single",
    rownames  = FALSE,
    options   = list(
      pageLength = 25, dom = "tp", scrollX = TRUE,
      columnDefs = list(list(orderable = FALSE, targets = 0)),
      language   = list(emptyTable = "No participants match filters.")
    ),
    class = "stripe hover")
  }

  output$ptable_1 <- render_priority_table(1L)
  output$ptable_2 <- render_priority_table(2L)
  output$ptable_3 <- render_priority_table(3L)
  output$ptable_4 <- render_priority_table(4L)

  # ── Row-click detail modal ────────────────────────────────────────────────
  lapply(1:4, function(pri) {
    observeEvent(input[[paste0("ptable_", pri, "_rows_selected")]], {
      p   <- filtered_participants() %>% filter(priority == pri)
      if (pri == 4L) p <- p %>% arrange(expected_completion)
      idx <- input[[paste0("ptable_", pri, "_rows_selected")]]
      if (is.null(idx) || length(idx) == 0) return()
      subj <- p[idx, ]
      sid  <- subj$subject_id[1]
      rv   <- get_review(sid)
      meta <- SITE_META[[subj$site_code[1]]]
      if (is.null(meta)) meta <- list(name="\u2014",city="\u2014",state="\u2014",pi="\u2014",coord="\u2014",color="#888")

      showModal(modalDialog(
        title     = sid,
        size      = "l",
        easyClose = TRUE,
        footer    = NULL,

        fluidRow(
          column(4,
            div(class="detail-section",
              div(class="detail-section-title","Demographics"),
              drow("Gender",    safe_val(subj$gender[1])),
              drow("Ethnicity", safe_val(subj$ethnicity[1])),
              drow("Education", safe_val(subj$education_level[1])),
              drow("BMI",       safe_val(subj$bmi[1])),
              drow("Smoker",    safe_val(subj$smoker[1]))
            )
          ),
          column(4,
            div(class="detail-section",
              div(class="detail-section-title","Site Info"),
              drow("Site Code", tags$span(style=sprintf("color:%s;font-weight:700",meta$color), safe_val(subj$site_code[1]))),
              drow("Site Name", meta$name),
              drow("Location",  sprintf("%s, %s", meta$city, meta$state)),
              drow("PI",        meta$pi),
              drow("Coordinator", meta$coord)
            )
          ),
          column(4,
            div(class="detail-section",
              div(class="detail-section-title","Study Status"),
              drow("Condition",       safe_val(subj$study_condition[1])),
              drow("Status",          safe_val(subj$study_status[1])),
              drow("Eligible",        safe_val(subj$overall_eligible[1])),
              drow("Exclusion",       safe_val(subj$exclusion_reason[1])),
              drow("Visits Done",     sprintf("%d / 5", subj$visits_completed[1])),
              drow("Missed Visits",   as.character(subj$visits_missed[1])),
              drow("Est. Completion", fmt_date(safe_val(subj$expected_completion[1], ""))),
              drow("Referral",        safe_val(subj$referring_source[1])),
              drow("Consent Date",    fmt_date(safe_val(subj$consent_date[1], "")))
            )
          )
        ),

        fluidRow(
          column(4,
            div(class="detail-section",
              div(class="detail-section-title","Baseline Vitals"),
              drow("Blood Pressure", sprintf("%s/%s mmHg", safe_val(subj$systolic_bp[1]),  safe_val(subj$diastolic_bp[1]))),
              drow("Heart Rate",     sprintf("%s bpm",     safe_val(subj$resting_heart_rate[1]))),
              drow("Stress Score",   sprintf("%s / 10",    safe_val(subj$stress_score_1_10[1]))),
              drow("Wellbeing",      sprintf("%s / 10",    safe_val(subj$wellbeing_score_1_10[1]))),
              drow("Avg Sleep",      sprintf("%s hrs",     safe_val(subj$avg_sleep_hours[1]))),
              drow("Exercise Days",  safe_val(subj$exercise_days_per_week[1]))
            )
          ),
          column(4,
            div(class="detail-section",
              div(class="detail-section-title","Lab Results"),
              drow("Normal",       tags$span(style="color:#3b6d11", as.character(subj$lab_normal[1]))),
              drow("Flagged High", tags$span(style="color:#a32d2d", as.character(subj$lab_high[1]))),
              drow("Flagged Low",  tags$span(style="color:#854f0b", as.character(subj$lab_low[1]))),
              drow("% Abnormal",   sprintf("%d%%", subj$pct_abnormal[1]))
            )
          ),
          column(4,
            div(class="detail-section",
              div(class="detail-section-title", sprintf("Adverse Events (%d)", subj$ae_count[1])),
              if (subj$ae_count[1] == 0)
                p(style="font-size:12px;color:#999891;font-style:italic;","None reported")
              else
                pre(style="font-size:12px;font-family:Georgia,serif;white-space:pre-wrap;background:none;border:none;padding:0;margin:0;color:#1a1a18;",
                    subj$ae_detail[1])
            )
          )
        ),

        hr(),

        div(class="review-box",
          div(class="review-box-title", "\u270e\u00a0 Data Review"),
          fluidRow(
            column(4,
              selectInput(paste0("rv_status_", sid), "Review Status",
                          choices  = c("Not Started","In Progress","Complete"),
                          selected = rv$status)
            ),
            column(8,
              textAreaInput(paste0("rv_notes_", sid), "Review Notes",
                            value       = rv$notes,
                            rows        = 3,
                            placeholder = "Add notes about this participant's data review\u2026")
            )
          ),
          if (rv$timestamp != "")
            p(style="font-size:11px;color:#999891;font-family:'Courier New',monospace;",
              sprintf("Last saved: %s \u00b7 Reviewer: %s", fmt_date(rv$timestamp), rv$reviewer))
          else
            NULL,
          actionButton(paste0("rv_save_", sid), "Save Review",
                       style="background:#f0eafa;color:#5b2d8e;border:0.5px solid #8b5cf6;border-radius:6px;font-size:12px;font-family:'Courier New',monospace;font-weight:600;padding:6px 14px;")
        )
      ))

      observeEvent(input[[paste0("rv_save_", sid)]], {
        save_review(sid,
                    input[[paste0("rv_status_", sid)]],
                    input[[paste0("rv_notes_",  sid)]])
        showNotification(sprintf("\u2713 Review saved for %s", sid), type="message", duration=2)
      }, ignoreInit=TRUE, once=FALSE)
    })
  })

  # ── Schedule tab ───────────────────────────────────────────────────────────
  output$schedule_ui <- renderUI({
    sched <- schedule_raw()
    if (is.null(sched)) {
      return(box(width=12,
        p(style="color:#999891;font-style:italic;font-size:13px;padding:2rem;text-align:center;",
          "No schedule data loaded. Upload mock_site_visit_schedule.csv on the Upload tab.")))
    }

    today   <- Sys.Date()
    siteF   <- input$sched_site
    typeF   <- input$sched_type
    statusF <- input$sched_status
    windowF <- input$sched_window

    rows <- sched %>% mutate(sched_d = suppressWarnings(as.Date(scheduled_date)))
    if (!is.null(siteF)   && siteF   != "") rows <- rows %>% filter(site_code  == siteF)
    if (!is.null(typeF)   && typeF   != "") rows <- rows %>% filter(visit_type == typeF)
    if (!is.null(statusF) && statusF != "") rows <- rows %>% filter(status     == statusF)
    if (!is.null(windowF) && windowF != "") {
      if (windowF == "past") {
        rows <- rows %>% filter(!is.na(sched_d), sched_d < today)
      } else {
        cutoff <- today + as.integer(windowF)
        rows   <- rows %>% filter(!is.na(sched_d), sched_d >= today, sched_d <= cutoff)
      }
    }

    if (nrow(rows) == 0) {
      return(box(width=12,
        p(style="color:#999891;font-style:italic;font-size:13px;padding:2rem;text-align:center;",
          "No visits match the current filters.")))
    }

    sites_present <- sort(unique(rows$site_code))
    ui_list <- lapply(sites_present, function(code) {
      sr   <- rows %>% filter(site_code == code) %>% arrange(sched_d)
      meta <- SITE_META[[code]]
      if (is.null(meta)) meta <- list(name=code, color="#888")

      status_col <- sapply(sr$status, function(s)
        switch(s,
          "Completed"   = pill_html(s, "#eaf3de","#3b6d11"),
          "Scheduled"   = pill_html(s, "#e6f1fb","#0c447c"),
          "Rescheduled" = pill_html(s, "#faeeda","#854f0b"),
          "Cancelled"   = pill_html(s, "#fcebeb","#a32d2d"),
          pill_html(s, "#f1efe8","#5f5e5a")
        ))

      type_col <- sapply(sr$visit_type, function(t)
        if (t == "Monitoring Visit") pill_html("Monitoring","#e0f4f4","#0e6b6b")
        else pill_html("Participant","#f1efe8","#5f5e5a"))

      modality_col <- ifelse(
        sr$visit_type == "Monitoring Visit",
        sapply(sr$cra,            function(x) safe_val(x)),
        sapply(sr$visit_modality, function(x) safe_val(x))
      )

      df <- data.frame(
        Type       = type_col,
        Visit      = sapply(sr$visit_name,      safe_val),
        Subject    = sapply(sr$subject_id,      function(x) safe_val(x, "\u2014")),
        Scheduled  = sapply(sr$scheduled_date,  fmt_date),
        Actual     = sapply(sr$actual_date,     fmt_date),
        Status     = status_col,
        `Modality / CRA` = modality_col,
        check.names = FALSE, stringsAsFactors = FALSE
      )

      has_notes <- "monitoring_notes" %in% names(sr) &&
                   any(nchar(trimws(sr$monitoring_notes)) > 0, na.rm=TRUE)
      if (has_notes) df[["Notes"]] <- sapply(sr$monitoring_notes, function(x) safe_val(x, ""))

      box(width=12,
        tags$div(
          style=sprintf("font-size:12px;font-family:'Courier New',monospace;font-weight:600;color:%s;text-transform:uppercase;letter-spacing:0.6px;margin-bottom:8px;display:flex;align-items:center;gap:8px;",
                        meta$color),
          tags$span(style=sprintf("width:10px;height:10px;border-radius:50%%;background:%s;display:inline-block;",meta$color)),
          sprintf("%s \u2014 %s \u00b7 %d visit%s", code, meta$name, nrow(sr), ifelse(nrow(sr)!=1,"s",""))
        ),
        DT::datatable(df, escape=FALSE, rownames=FALSE, selection="none",
                      options=list(dom="t", pageLength=nrow(df), scrollX=TRUE),
                      class="stripe hover")
      )
    })
    do.call(tagList, ui_list)
  })

  # ── Export ─────────────────────────────────────────────────────────────────
  output$download_review <- downloadHandler(
    filename = function() "mock_review_status.csv",
    content  = function(file) {
      req(participants())
      p  <- participants()
      rv <- review_state()
      out <- p %>%
        select(subject_id, site_code, study_condition, study_status) %>%
        left_join(rv, by = "subject_id") %>%
        mutate(
          review_status = tidyr::replace_na(review_status, "Not Started"),
          review_notes  = tidyr::replace_na(review_notes,  ""),
          reviewer      = tidyr::replace_na(reviewer,      ""),
          last_updated  = tidyr::replace_na(last_updated,  "")
        ) %>%
        select(subject_id, site_code, study_condition, study_status,
               review_status, review_notes, reviewer, last_updated)
      write_csv(out, file)
    }
  )
}

shinyApp(ui, server)
