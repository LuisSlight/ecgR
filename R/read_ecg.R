#' Read a MIT-BIH ECG record into a tidy data frame
#'
#' @param record_path Path to the record WITHOUT file extension.
#'   e.g. "D:/data/mitbih/100" (not "100.dat")
#' @param from_sec Start time in seconds. Default 0.
#' @param to_sec End time in seconds. NULL reads the full record.
#' @param lead Lead name(s) to return. NULL returns all leads.
#'   e.g. "MLII" or c("MLII", "V5")
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item \code{time_sec}: time in seconds
#'     \item \code{MLII}, \code{V5}, etc.: signal amplitude in mV
#'     \item \code{annotation}: beat label at R-peak positions, NA elsewhere.
#'       Common symbols: N = normal, A = atrial premature beat,
#'       V = premature ventricular contraction
#'   }
#'
#' @examples
#' \dontrun{
#' df <- read_ecg("D:/data/mitbih/100")
#' df <- read_ecg("D:/data/mitbih/100", from_sec = 0, to_sec = 10)
#' df <- read_ecg("D:/data/mitbih/100", lead = "MLII")
#' }
#'
#' @export
read_ecg <- function(record_path,
                     from_sec = 0,
                     to_sec   = NULL,
                     lead     = NULL) {

  # Check dependencies
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required. Install with: install.packages('reticulate')")
  }

  wfdb <- tryCatch(
    reticulate::import("wfdb"),
    error = function(e) {
      stop(
        "Python package 'wfdb' not found.\n",
        "Install it with: reticulate::py_install('wfdb')"
      )
    }
  )

  # Read header to get sampling frequency and lead names
  header    <- wfdb$rdheader(record_path)
  fs        <- header$fs
  sig_names <- header$sig_name

  # Compute sample range
  sampfrom <- as.integer(from_sec * fs)
  sampto   <- if (is.null(to_sec)) NULL else as.integer(to_sec * fs)

  # Read signal
  if (is.null(sampto)) {
    record <- wfdb$rdrecord(record_path, sampfrom = sampfrom)
  } else {
    record <- wfdb$rdrecord(record_path, sampfrom = sampfrom, sampto = sampto)
  }

  n <- nrow(record$p_signal)

  # Build signal data frame
  df           <- as.data.frame(record$p_signal)
  colnames(df) <- sig_names
  df           <- cbind(time_sec = (seq_len(n) - 1L) / fs + from_sec, df)
  df$annotation <- NA_character_

  # Read and merge annotations
  ann_file <- paste0(record_path, ".atr")
  if (file.exists(ann_file)) {
    ann <- tryCatch({
      if (is.null(sampto)) {
        wfdb$rdann(record_path, "atr", sampfrom = sampfrom)
      } else {
        wfdb$rdann(record_path, "atr", sampfrom = sampfrom, sampto = sampto)
      }
    }, error = function(e) NULL)

    if (!is.null(ann) && length(ann$sample) > 0) {
      # Python is 0-indexed; convert to R 1-indexed row positions
      ann_rows    <- as.integer(ann$sample) - sampfrom + 1L
      ann_symbols <- as.character(ann$symbol)
      valid       <- ann_rows >= 1L & ann_rows <= n
      df$annotation[ann_rows[valid]] <- ann_symbols[valid]
    }
  }

  # Subset leads if requested
  if (!is.null(lead)) {
    missing_leads <- setdiff(lead, sig_names)
    if (length(missing_leads) > 0) {
      warning(
        "Lead(s) not found in record: ", paste(missing_leads, collapse = ", "),
        ". Available: ", paste(sig_names, collapse = ", ")
      )
      lead <- intersect(lead, sig_names)
    }
    df <- df[, c("time_sec", lead, "annotation")]
  }

  # Attach metadata as attributes for use by downstream functions
  attr(df, "fs")     <- fs
  attr(df, "record") <- basename(record_path)
  attr(df, "leads")  <- if (is.null(lead)) sig_names else lead

  class(df) <- c("ecg_df", "data.frame")
  df
}


#' Print basic information about an ECG data frame
#'
#' @param x An \code{ecg_df} object returned by \code{read_ecg()}.
#' @param ... Further arguments passed to \code{print.data.frame}.
#' @export
print.ecg_df <- function(x, ...) {
  cat("Record :", attr(x, "record"), "\n")
  cat("Leads  :", paste(attr(x, "leads"), collapse = ", "), "\n")
  cat("Fs     :", attr(x, "fs"), "Hz\n")
  cat("Rows   :", nrow(x), "\n")
  counts <- table(x$annotation[!is.na(x$annotation)])
  if (length(counts) > 0) {
    cat("Beats  :", paste(names(counts), as.integer(counts), sep = "=", collapse = ", "), "\n")
  }
  invisible(x)
}

#' MIT-BIH annotation symbol reference table
#'
#' Returns a data frame describing all standard MIT-BIH beat annotation
#' symbols, their plain-English labels, and clinical categories.
#' Useful for building UI tooltips or report legends.
#'
#' @return A data frame with columns \code{symbol}, \code{label},
#'   and \code{category}.
#'
#' @examples
#' ecg_annotation_legend()
#'
#' @export
ecg_annotation_legend <- function() {
  data.frame(
    symbol = c("N","L","R","A","a","J","S","V","F","e","j","E","/","f","Q","~"),
    label = c(
      "Normal beat",
      "Left bundle branch block beat",
      "Right bundle branch block beat",
      "Atrial premature beat",
      "Aberrated atrial premature beat",
      "Nodal (junctional) premature beat",
      "Supraventricular premature beat",
      "Premature ventricular contraction",
      "Fusion of ventricular and normal beat",
      "Atrial escape beat",
      "Nodal (junctional) escape beat",
      "Ventricular escape beat",
      "Paced beat",
      "Fusion of paced and normal beat",
      "Unclassifiable beat",
      "Signal quality change"
    ),
    category = c(
      "Normal", "Conduction", "Conduction",
      "Supraventricular", "Supraventricular", "Supraventricular", "Supraventricular",
      "Ventricular", "Ventricular",
      "Escape", "Escape", "Escape",
      "Paced", "Paced",
      "Other", "Noise"
    ),
    stringsAsFactors = FALSE
  )
}
