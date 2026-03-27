#' Launch the ecgR Shiny app
#' @export
launch_app <- function() {

  css <- "
    body { background: #F8FAFC; font-family: 'Segoe UI', sans-serif; }
    .main-header {
      background: linear-gradient(135deg, #1565C0, #0D47A1);
      color: white; padding: 18px 28px 14px;
    }
    .main-header h2 { margin: 0; font-size: 22px; font-weight: 600; }
    .main-header p  { margin: 4px 0 0; font-size: 13px; opacity: 0.8; }
    .sidebar-panel {
      background: white; border-radius: 10px;
      padding: 20px; box-shadow: 0 1px 6px rgba(0,0,0,0.08);
      margin: 16px 8px 16px 16px;
    }
    .sidebar-panel h4 {
      color: #1565C0; font-size: 13px; font-weight: 600;
      text-transform: uppercase; letter-spacing: 0.05em; margin-bottom: 14px;
    }
    .main-content { margin: 16px 16px 16px 8px; }
    .metric-card {
      background: white; border-radius: 10px; padding: 16px 20px;
      box-shadow: 0 1px 6px rgba(0,0,0,0.08); margin-bottom: 12px;
      border-left: 4px solid #ccc;
    }
    .metric-card.normal  { border-left-color: #43A047; }
    .metric-card.warning { border-left-color: #FB8C00; }
    .metric-card.alert   { border-left-color: #E53935; }
    .metric-card .metric-name  {
      font-size: 12px; font-weight: 600; color: #546E7A;
      text-transform: uppercase; letter-spacing: 0.05em;
    }
    .metric-card .metric-value {
      font-size: 28px; font-weight: 700; color: #1A237E; line-height: 1.2;
    }
    .metric-card .metric-unit  { font-size: 13px; color: #78909C; }
    .metric-card .metric-desc  {
      font-size: 12px; color: #607D8B; margin-top: 6px; line-height: 1.5;
    }
    .beat-badge {
      display: inline-block; border-radius: 4px; padding: 2px 8px;
      font-size: 12px; font-weight: 600; margin: 3px 4px 3px 0;
    }
    .badge-normal  { background: #E8F5E9; color: #2E7D32; }
    .badge-warning { background: #FFF3E0; color: #E65100; }
    .badge-alert   { background: #FFEBEE; color: #B71C1C; }
    .info-box {
      background: #E3F2FD; border-radius: 8px; padding: 12px 16px;
      font-size: 13px; color: #1565C0; margin-bottom: 16px;
      border-left: 3px solid #1565C0;
    }
    .status-box {
      background: #F1F8E9; border-radius: 8px; padding: 12px;
      font-size: 12px; font-family: monospace; color: #33691E; margin-top: 12px;
    }
    .load-btn {
      background: #1565C0 !important; border: none !important;
      border-radius: 8px !important; font-weight: 600 !important;
      padding: 10px !important; width: 100%;
    }
    hr { border-color: #ECEFF1; margin: 16px 0; }
  "

  # ── UI ──────────────────────────────────────────────────────────────────
  ui <- shiny::tagList(
    shiny::tags$head(shiny::tags$style(shiny::HTML(css))),

    shiny::div(class = "main-header",
               shiny::h2("ecgR — ECG Explorer"),
               shiny::p("Visualize and analyze electrocardiogram data")
    ),

    shiny::fluidRow(

      # Sidebar
      shiny::column(3,
                    shiny::div(class = "sidebar-panel",

                               shiny::h4("Data source"),
                               shiny::radioButtons("data_source", label = NULL,
                                                   choices  = c("MIT-BIH (.dat/.hea)" = "mitbih", "CSV upload" = "csv"),
                                                   selected = "mitbih", inline = TRUE
                               ),
                               shiny::hr(),

                               shiny::conditionalPanel(
                                 condition = "input.data_source == 'mitbih'",
                                 shiny::textInput("record_path", "Record path (no extension)",
                                                  placeholder = "e.g. D:/data/mitbih/100")
                               ),

                               shiny::conditionalPanel(
                                 condition = "input.data_source == 'csv'",
                                 shiny::fileInput("csv_file", "Upload CSV file",
                                                  accept = c(".csv", "text/csv")),
                                 shiny::textInput("time_col", "Time column name",
                                                  placeholder = "e.g. time  (leave blank if none)"),
                                 shiny::textInput("signal_col", "Signal column name",
                                                  placeholder = "e.g. ecg, voltage, lead_I"),
                                 shiny::numericInput("fs_input", "Sampling rate (Hz)", value = 360, min = 1)
                               ),

                               shiny::hr(),
                               shiny::h4("Time window"),
                               shiny::fluidRow(
                                 shiny::column(6, shiny::numericInput("from_sec", "From (s)", 0, 0)),
                                 shiny::column(6, shiny::numericInput("to_sec",   "To (s)",  60, 1))
                               ),

                               shiny::conditionalPanel(
                                 condition = "input.data_source == 'mitbih'",
                                 shiny::selectInput("lead", "Lead",
                                                    choices = c("MLII", "V5"), selected = "MLII")
                               ),

                               shiny::actionButton("load_btn", "Load & Analyze",
                                                   class = "btn btn-primary load-btn"),

                               shiny::hr(),
                               shiny::uiOutput("status_ui")
                    )
      ),

      # Main panel
      shiny::column(9,
                    shiny::div(class = "main-content",
                               shiny::tabsetPanel(

                                 shiny::tabPanel("ECG waveform",
                                                 shiny::div(class = "tab-content",
                                                            shiny::div(class = "info-box",
                                                                       shiny::strong("What am I looking at? "),
                                                                       "Each spike is one heartbeat — the sharp peak is the QRS complex.
                  Blue circles mark normal beats. Orange = atrial premature beat
                  (upper chamber fired early). Red = ventricular premature
                  contraction (lower chamber fired early)."
                                                            ),
                                                            shiny::plotOutput("ecg_plot", height = "360px")
                                                 )
                                 ),

                                 shiny::tabPanel("RR intervals",
                                                 shiny::div(class = "tab-content",
                                                            shiny::div(class = "info-box",
                                                                       shiny::strong("What am I looking at? "),
                                                                       "Each dot is the time gap between two consecutive heartbeats.
                  A healthy heart shows gentle natural variation — the blue band
                  is the normal range. Red diamonds far outside the band indicate
                  irregular beats: a short gap (early beat) always followed by a
                  long gap (compensatory pause)."
                                                            ),
                                                            shiny::plotOutput("rr_plot", height = "360px")
                                                 )
                                 ),

                                 shiny::tabPanel("HRV summary",
                                                 shiny::div(class = "tab-content",
                                                            shiny::div(class = "info-box",
                                                                       shiny::strong("Heart Rate Variability (HRV) "),
                                                                       "measures how much the time between beats naturally varies.
                  Higher HRV generally indicates a healthy, responsive autonomic
                  nervous system. Metrics are computed from the full record."
                                                            ),
                                                            shiny::uiOutput("hrv_cards"),
                                                            shiny::br(),
                                                            shiny::uiOutput("beat_summary"),
                                                            shiny::br(),
                                                            shiny::uiOutput("interpretation_ui")
                                                 )
                                 )

                               )
                    )
      )
    )
  )

  # ── Server ──────────────────────────────────────────────────────────────
  server <- function(input, output, session) {

    ecg_data <- shiny::eventReactive(input$load_btn, {
      shiny::withProgress(message = "Reading signal...", {
        tryCatch({
          if (input$data_source == "mitbih") {
            shiny::req(input$record_path)
            read_ecg(input$record_path,
                     from_sec = input$from_sec,
                     to_sec   = input$to_sec)
          } else {
            shiny::req(input$csv_file)
            tc <- if (nchar(trimws(input$time_col)) > 0) trimws(input$time_col) else NULL
            sc <- if (nchar(trimws(input$signal_col)) > 0) trimws(input$signal_col) else NULL
            read_ecg_csv(input$csv_file$datapath,
                         time_col = tc, signal_cols = sc,
                         fs = input$fs_input,
                         from_sec = input$from_sec, to_sec = input$to_sec)
          }
        }, error = function(e) {
          shiny::showNotification(paste("Error:", conditionMessage(e)),
                                  type = "error", duration = 10)
          NULL
        })
      })
    })

    hrv_data <- shiny::eventReactive(input$load_btn, {
      shiny::withProgress(message = "Computing HRV...", {
        tryCatch({
          if (input$data_source == "mitbih") {
            shiny::req(input$record_path)
            df_full <- read_ecg(input$record_path)
          } else {
            shiny::req(input$csv_file)
            tc <- if (nchar(trimws(input$time_col)) > 0) trimws(input$time_col) else NULL
            sc <- if (nchar(trimws(input$signal_col)) > 0) trimws(input$signal_col) else NULL
            df_full <- read_ecg_csv(input$csv_file$datapath,
                                    time_col = tc, signal_cols = sc,
                                    fs = input$fs_input)
          }
          compute_hrv(df_full)
        }, error = function(e) NULL)
      })
    })

    features_data <- shiny::eventReactive(input$load_btn, {
      shiny::withProgress(message = "Extracting ECG features...", {
        tryCatch({
          if (input$data_source == "mitbih") {
            shiny::req(input$record_path)
            df_full <- read_ecg(input$record_path)
          } else {
            shiny::req(input$csv_file)
            tc <- if (nchar(trimws(input$time_col)) > 0) trimws(input$time_col) else NULL
            sc <- if (nchar(trimws(input$signal_col)) > 0) trimws(input$signal_col) else NULL
            df_full <- read_ecg_csv(
              input$csv_file$datapath,
              time_col = tc,
              signal_cols = sc,
              fs = input$fs_input
            )
          }

          extract_ecg_features(df_full)

        }, error = function(e) {
          shiny::showNotification(
            paste("Feature extraction error:", conditionMessage(e)),
            type = "error",
            duration = 10
          )
          NULL
        })
      })
    })

    output$status_ui <- shiny::renderUI({
      df <- ecg_data()
      if (is.null(df)) return(shiny::p("No record loaded.",
                                       style = "color:#90A4AE; font-size:13px; margin-top:8px;"))
      shiny::div(class = "status-box",
                 paste0("Record : ", attr(df, "record")), shiny::br(),
                 paste0("Leads  : ", paste(attr(df, "leads"), collapse = ", ")), shiny::br(),
                 paste0("Fs     : ", attr(df, "fs"), " Hz"), shiny::br(),
                 paste0("Rows   : ", nrow(df))
      )
    })

    output$ecg_plot <- shiny::renderPlot({
      df <- ecg_data()
      shiny::req(df)

      lead <- attr(df, "leads")[1]

      if (!is.null(input$lead) && input$lead %in% names(df)) {
        lead <- input$lead
      }

      plot_ecg(df, lead = lead)
    })

    output$rr_plot <- shiny::renderPlot({
      hrv <- hrv_data(); shiny::req(hrv)
      plot_rr(hrv)
    })

    output$hrv_cards <- shiny::renderUI({
      hrv <- hrv_data(); shiny::req(hrv)

      hr_class    <- ifelse(hrv$mean_hr < 60 | hrv$mean_hr > 100, "warning", "normal")
      sdnn_class  <- ifelse(hrv$sdnn  < 20, "alert", ifelse(hrv$sdnn  < 50, "warning", "normal"))
      rmssd_class <- ifelse(hrv$rmssd < 20, "alert", ifelse(hrv$rmssd < 40, "warning", "normal"))
      pnn50_class <- ifelse(hrv$pnn50 < 3, "warning", "normal")

      card <- function(name, value, unit, desc, cls) {
        shiny::div(class = paste("metric-card", cls),
                   shiny::div(class = "metric-name", name),
                   shiny::div(
                     shiny::span(class = "metric-value", value),
                     shiny::span(class = "metric-unit", paste0(" ", unit))
                   ),
                   shiny::div(class = "metric-desc", desc)
        )
      }

      shiny::fluidRow(
        shiny::column(3, card("Mean heart rate", hrv$mean_hr, "bpm",
                              "Normal: 60–100 bpm. Average beats per minute over the full recording.", hr_class)),
        shiny::column(3, card("SDNN", hrv$sdnn, "ms",
                              "Standard deviation of RR intervals. Above 50 ms is generally healthy.", sdnn_class)),
        shiny::column(3, card("RMSSD", hrv$rmssd, "ms",
                              "Short-term HRV. Reflects autonomic nervous system responsiveness.", rmssd_class)),
        shiny::column(3, card("pNN50", hrv$pnn50, "%",
                              "% of beat pairs differing by >50 ms. Marker of parasympathetic activity.", pnn50_class))
      )
    })

    output$beat_summary <- shiny::renderUI({
      hrv <- hrv_data(); shiny::req(hrv)
      legend <- ecg_annotation_legend()
      counts <- as.data.frame(hrv$beat_counts)
      colnames(counts) <- c("symbol", "count")
      merged <- merge(counts, legend[, c("symbol","label","category")],
                      by = "symbol", all.x = TRUE)
      merged$label[is.na(merged$label)] <- "Unknown"
      merged <- merged[order(-merged$count), ]

      badge_cls <- function(cat) {
        if (is.na(cat) || cat == "Noise") return("beat-badge badge-warning")
        if (cat == "Normal") return("beat-badge badge-normal")
        "beat-badge badge-alert"
      }

      badges <- lapply(seq_len(nrow(merged)), function(i) {
        shiny::span(class = badge_cls(merged$category[i]),
                    paste0(merged$label[i], ": ", merged$count[i]))
      })

      shiny::div(
        shiny::h4("Beat composition",
                  style = "color:#37474F; font-size:14px; font-weight:600; margin-bottom:10px;"),
        shiny::div(badges)
      )
    })

    output$interpretation_ui <- shiny::renderUI({
      features <- features_data()
      shiny::req(features)

      interp <- interpret_ecg_features(features)

      shiny::div(
        shiny::h4("Interpretation",
                  style = "color:#37474F; font-size:14px; font-weight:600; margin-bottom:10px;"),
        shiny::div(
          style = "font-size:11px; color:#90A4AE; margin-bottom:12px; font-style:italic;",
          "For educational purposes only. This is not a medical diagnosis."
        ),

        # Summary block
        shiny::div(
          style = paste0(
            "background:#E3F2FD; border-left:3px solid #1565C0;",
            "border-radius:6px; padding:12px 14px; margin-bottom:12px;",
            "font-size:13px; color:#0D47A1; line-height:1.6; font-weight:500;"
          ),
          interp$summary
        ),

        # Detail bullets
        shiny::tags$ul(
          style = "padding-left:20px; margin-bottom:0;",
          lapply(interp$details, function(txt) {
            shiny::tags$li(
              style = "margin-bottom:8px; font-size:13px; color:#455A64; line-height:1.6;",
              txt
            )
          })
        )
      )
    })



  }

  shiny::shinyApp(ui = ui, server = server)
}
