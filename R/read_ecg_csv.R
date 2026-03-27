#' Read an ECG signal from a CSV file
#'
#' Reads a plain CSV file containing ECG data and returns the same
#' \code{ecg_df} format as \code{read_ecg()}, so all downstream
#' functions (plot_ecg, compute_hrv, plot_rr) work without any changes.
#'
#' @param file_path Path to the CSV file.
#' @param time_col  Name of the time column. If NULL, time is generated
#'   automatically from \code{fs}.
#' @param signal_cols Character vector of signal column names to keep.
#'   If NULL, all non-time numeric columns are used.
#' @param fs Sampling frequency in Hz. Required if \code{time_col} is NULL.
#'   If \code{time_col} is provided, fs is inferred automatically.
#' @param from_sec Start time in seconds. Default 0.
#' @param to_sec   End time in seconds. NULL = full file.
#'
#' @return An \code{ecg_df} object identical in structure to \code{read_ecg()}.
#'
#' @examples
#' \dontrun{
#' # CSV with a time column
#' df <- read_ecg_csv("my_ecg.csv", time_col = "time", signal_cols = "ecg")
#'
#' # CSV with no time column, 500 Hz Apple Watch export
#' df <- read_ecg_csv("apple_watch.csv", signal_cols = "voltage", fs = 500)
#'
#' # Then use exactly like MIT-BIH data
#' plot_ecg(df)
#' hrv <- compute_hrv(df)
#' }
#'
#' @export
read_ecg_csv <- function(file_path,
                         time_col     = NULL,
                         signal_cols  = NULL,
                         fs           = NULL,
                         from_sec     = 0,
                         to_sec       = NULL) {

  # ── 1. Read file ───────────────────────────────────────────────────────
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  raw <- utils::read.csv(file_path, stringsAsFactors = FALSE)

  if (nrow(raw) == 0) stop("CSV file is empty.")

  # ── 2. Identify signal columns ─────────────────────────────────────────
  numeric_cols <- names(raw)[sapply(raw, is.numeric)]

  if (!is.null(time_col)) {
    if (!time_col %in% names(raw)) {
      stop("time_col '", time_col, "' not found. Available columns: ",
           paste(names(raw), collapse = ", "))
    }
    numeric_cols <- setdiff(numeric_cols, time_col)
  }

  if (is.null(signal_cols)) {
    signal_cols <- numeric_cols
    if (length(signal_cols) == 0) stop("No numeric signal columns found.")
    message("Using signal columns: ", paste(signal_cols, collapse = ", "))
  } else {
    missing <- setdiff(signal_cols, names(raw))
    if (length(missing) > 0) {
      stop("Signal column(s) not found: ", paste(missing, collapse = ", "),
           "\nAvailable: ", paste(names(raw), collapse = ", "))
    }
  }

  # ── 3. Build time vector ───────────────────────────────────────────────
  if (!is.null(time_col)) {
    time_vec <- as.numeric(raw[[time_col]])
    # Infer fs from median time step
    dt <- median(diff(time_vec), na.rm = TRUE)
    if (is.null(fs)) {
      fs <- round(1 / dt)
      message("Inferred sampling rate: ", fs, " Hz")
    }
  } else {
    if (is.null(fs)) {
      stop("Please provide 'fs' (sampling frequency in Hz) ",
           "when there is no time column in the CSV.")
    }
    time_vec <- (seq_len(nrow(raw)) - 1L) / fs
  }

  # ── 4. Apply time range filter ─────────────────────────────────────────
  keep <- time_vec >= from_sec
  if (!is.null(to_sec)) keep <- keep & time_vec <= to_sec

  if (sum(keep) == 0) {
    stop("No data in the requested time range [", from_sec, ", ", to_sec, "] s.")
  }

  time_vec <- time_vec[keep]
  raw      <- raw[keep, , drop = FALSE]

  # ── 5. Assemble ecg_df ─────────────────────────────────────────────────
  df           <- raw[, signal_cols, drop = FALSE]
  df           <- cbind(time_sec = time_vec, df)
  df$annotation <- NA_character_

  rownames(df) <- NULL

  attr(df, "fs")     <- fs
  attr(df, "record") <- tools::file_path_sans_ext(basename(file_path))
  attr(df, "leads")  <- signal_cols

  class(df) <- c("ecg_df", "data.frame")
  df
}
