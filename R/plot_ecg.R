#' Plot an ECG waveform with annotated beats
#'
#' @param ecg_df An \code{ecg_df} object from \code{read_ecg()}.
#' @param lead Which lead to plot. Default "MLII".
#' @param show_annotations If TRUE, mark beat positions with colored points.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' df <- read_ecg("data/100", from_sec = 0, to_sec = 10)
#' plot_ecg(df)
#' plot_ecg(df, lead = "V5")
#' }
#'
#' @export
plot_ecg <- function(ecg_df,
                     lead             = "MLII",
                     show_annotations = TRUE) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Install with: install.packages('ggplot2')")
  }

  if (!lead %in% names(ecg_df)) {
    stop("Lead '", lead, "' not found. Available: ",
         paste(attr(ecg_df, "leads"), collapse = ", "))
  }

  ann_colors <- c(
    "N" = "#2196F3",
    "A" = "#FF9800",
    "V" = "#F44336",
    "L" = "#9C27B0",
    "R" = "#9C27B0",
    "+" = "#9E9E9E"
  )

  ann_labels <- c(
    "N" = "Normal",
    "A" = "Atrial premature",
    "V" = "Ventricular premature",
    "L" = "Left BBB",
    "R" = "Right BBB",
    "+" = "Rhythm marker"
  )

  p <- ggplot2::ggplot(ecg_df,
                       ggplot2::aes(x = time_sec, y = .data[[lead]])) +
    ggplot2::geom_line(color = "#37474F", linewidth = 0.4) +
    ggplot2::labs(
      title = paste0("ECG — Record ", attr(ecg_df, "record"),
                     "  |  Lead ", lead),
      x     = "Time (s)",
      y     = "Amplitude (mV)",
      color = "Beat type"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title       = ggplot2::element_text(size = 13, face = "bold")
    )

  if (show_annotations) {
    df_ann <- ecg_df[!is.na(ecg_df$annotation), ]

    if (nrow(df_ann) > 0) {
      df_ann$ann_label <- ifelse(
        df_ann$annotation %in% names(ann_colors),
        df_ann$annotation,
        "Other"
      )

      present_colors <- ann_colors[names(ann_colors) %in% df_ann$ann_label]
      present_labels <- ann_labels[names(ann_labels) %in% df_ann$ann_label]

      p <- p +
        ggplot2::geom_point(
          data = df_ann,
          ggplot2::aes(x = time_sec, y = .data[[lead]],
                       color = ann_label),
          size   = 2.5,
          shape  = 21,
          fill   = "white",
          stroke = 1.2
        ) +
        ggplot2::scale_color_manual(
          values = present_colors,
          labels = present_labels
        )
    }
  }

  p
}
