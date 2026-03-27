#' Plot RR intervals over time
#'
#' @param hrv An \code{ecg_hrv} object from \code{compute_hrv()}.
#' @param highlight_threshold Numeric. Flag RR intervals deviating more
#'   than this many ms from the median. Default 150.
#'
#' @return A ggplot2 object.
#' @export
plot_rr <- function(hrv, highlight_threshold = 150) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Install with: install.packages('ggplot2')")
  }

  rr     <- hrv$rr_intervals
  med_rr <- median(rr$rr_ms)
  rr$flag <- abs(rr$rr_ms - med_rr) > highlight_threshold

  # Smooth trend line via rolling mean (window = 10 beats)
  rr$trend <- stats::filter(rr$rr_ms, rep(1/10, 10), sides = 2)

  p <- ggplot2::ggplot(rr, ggplot2::aes(x = time_sec, y = rr_ms)) +

    # Shaded normal band (median ± 100ms)
    ggplot2::annotate("rect",
                      xmin = -Inf, xmax = Inf,
                      ymin = med_rr - 100, ymax = med_rr + 100,
                      fill = "#E3F2FD", alpha = 0.6
    ) +

    # Normal points
    ggplot2::geom_point(
      data = rr[!rr$flag, ],
      ggplot2::aes(x = time_sec, y = rr_ms),
      color = "#90CAF9", size = 0.8, alpha = 0.7
    ) +

    # Trend line
    ggplot2::geom_line(
      ggplot2::aes(y = trend),
      color = "#1565C0", linewidth = 0.7, na.rm = TRUE
    ) +

    # Median line
    ggplot2::geom_hline(
      yintercept = med_rr,
      linetype = "dashed", color = "#78909C", linewidth = 0.4
    ) +

    # Flagged anomalies
    ggplot2::geom_point(
      data = rr[rr$flag, ],
      ggplot2::aes(x = time_sec, y = rr_ms),
      color = "#E53935", size = 3, shape = 18, alpha = 0.9
    ) +

    ggplot2::annotate("text",
                      x     = max(rr$time_sec) * 0.01,
                      y     = med_rr + 115,
                      label = paste0("Median: ", round(med_rr), " ms"),
                      color = "#78909C", size = 3.2, hjust = 0
    ) +

    ggplot2::labs(
      title    = "RR intervals over time",
      subtitle = paste0(
        "Mean HR: ", hrv$mean_hr, " bpm  |  ",
        "SDNN: ",   hrv$sdnn,    " ms  |  ",
        "RMSSD: ",  hrv$rmssd,   " ms  |  ",
        "pNN50: ",  hrv$pnn50,   "%"
      ),
      x = "Time (s)",
      y = "RR interval (ms)"
    ) +

    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "#ECEFF1"),
      plot.subtitle    = ggplot2::element_text(size = 10, color = "#546E7A"),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA)
    )

  p
}
