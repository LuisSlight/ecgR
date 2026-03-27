#' Compute heart rate and HRV metrics from an ECG data frame
#'
#' @param ecg_df An \code{ecg_df} object from \code{read_ecg()}.
#' @param exclude_non_beats If TRUE, exclude rhythm markers ("+", "~")
#'   from RR interval calculation. Default TRUE.
#'
#' @return A list with the following elements:
#'   \itemize{
#'     \item \code{rr_intervals}: data frame of beat positions and RR intervals
#'     \item \code{mean_hr}: mean heart rate in beats per minute
#'     \item \code{sdnn}: standard deviation of RR intervals (ms)
#'     \item \code{rmssd}: root mean square of successive RR differences (ms)
#'     \item \code{pnn50}: proportion of successive RR differences > 50ms
#'     \item \code{beat_counts}: table of annotation symbol counts
#'   }
#'
#' @examples
#' \dontrun{
#' df  <- read_ecg("data/100")
#' hrv <- compute_hrv(df)
#' print(hrv)
#' }
#'
#' @export
compute_hrv <- function(ecg_df, exclude_non_beats = TRUE) {

  # Pull out only annotated rows
  df_ann <- ecg_df[!is.na(ecg_df$annotation), ]

  if (nrow(df_ann) < 2) {
    stop("Not enough annotated beats to compute HRV (need at least 2).")
  }

  # Optionally remove non-beat markers
  non_beat_symbols <- c("+", "~", "|", "s", "T", "*", "D", "=", "\"", "@")
  if (exclude_non_beats) {
    df_ann <- df_ann[!df_ann$annotation %in% non_beat_symbols, ]
  }

  if (nrow(df_ann) < 2) {
    stop("Not enough beat annotations after filtering non-beat symbols.")
  }

  # RR intervals in milliseconds
  rr_ms <- diff(df_ann$time_sec) * 1000

  rr_df <- data.frame(
    beat_index  = seq_len(nrow(df_ann) - 1),
    time_sec    = df_ann$time_sec[-nrow(df_ann)],   # time of current beat
    annotation  = df_ann$annotation[-nrow(df_ann)],
    rr_ms       = rr_ms
  )

  # Core HRV metrics
  mean_hr <- 60000 / mean(rr_ms)          # beats per minute
  sdnn    <- sd(rr_ms)                     # ms
  rmssd   <- sqrt(mean(diff(rr_ms)^2))    # ms
  pnn50   <- mean(abs(diff(rr_ms)) > 50)  # proportion

  result <- list(
    rr_intervals = rr_df,
    mean_hr      = round(mean_hr, 1),
    sdnn         = round(sdnn, 2),
    rmssd        = round(rmssd, 2),
    pnn50        = round(pnn50 * 100, 1),   # as percentage
    beat_counts  = table(ecg_df$annotation[!is.na(ecg_df$annotation)])
  )

  class(result) <- "ecg_hrv"
  result
}


#' Print a summary of HRV results
#'
#' @param x An \code{ecg_hrv} object from \code{compute_hrv()}.
#' @param ... Ignored.
#' @export
print.ecg_hrv <- function(x, ...) {
  cat("=== HRV Summary ===\n")
  cat("Mean heart rate :", x$mean_hr, "bpm\n")
  cat("SDNN            :", x$sdnn, "ms\n")
  cat("RMSSD           :", x$rmssd, "ms\n")
  cat("pNN50           :", x$pnn50, "%\n")
  cat("\nBeat counts:\n")
  print(x$beat_counts)
  invisible(x)
}
