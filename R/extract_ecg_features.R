#' Extract ECG features from a processed ECG data frame
#'
#' @param ecg_df A data frame returned by read_ecg() or read_ecg_csv().
#' @return A structured list of ECG features.
#' @export
extract_ecg_features <- function(ecg_df) {
  stopifnot(is.data.frame(ecg_df))

  # ---- metadata ----
  fs <- attr(ecg_df, "fs")
  leads <- attr(ecg_df, "leads")
  record <- attr(ecg_df, "record")

  if (is.null(fs)) {
    stop("Sampling rate (attr(ecg_df, 'fs')) is missing.")
  }

  duration_sec <- nrow(ecg_df) / fs

  # ---- existing HRV / rhythm features ----
  hrv <- compute_hrv(ecg_df)

  # ---- placeholder submodules ----
  signal_quality <- compute_signal_quality(ecg_df)
  qrs_features   <- compute_qrs_features(ecg_df)
  pwave_features <- compute_pwave_features(ecg_df)
  st_features    <- compute_st_features(ecg_df)
  qt_features    <- compute_qt_features(ecg_df)

  # ---- structured output ----
  out <- list(
    meta = list(
      record = record,
      fs = fs,
      leads = leads,
      n_samples = nrow(ecg_df),
      duration_sec = duration_sec
    ),
    rate_rhythm = list(
      mean_hr = hrv$mean_hr,
      sdnn = hrv$sdnn,
      rmssd = hrv$rmssd,
      pnn50 = hrv$pnn50,
      beat_counts = hrv$beat_counts
    ),
    signal_quality = signal_quality,
    morphology = list(
      qrs = qrs_features,
      p_wave = pwave_features,
      st_segment = st_features
    ),
    intervals = qt_features
  )

  class(out) <- c("ecg_features", class(out))
  out
}

# ---- placeholder helpers ----

compute_signal_quality <- function(ecg_df) {

  # choose first lead
  lead <- attr(ecg_df, "leads")[1]
  signal <- ecg_df[[lead]]

  n <- length(signal)

  # usable signal fraction
  usable_fraction <- sum(!is.na(signal)) / n

  # remove NA
  signal <- signal[!is.na(signal)]

  # baseline wander estimate
  baseline_wander <- stats::sd(signal)

  # high-frequency noise estimate
  hf_noise <- stats::sd(diff(signal))

  # simple quality grade
  quality_grade <- "good"

  if (usable_fraction < 0.9) {
    quality_grade <- "poor"
  } else if (hf_noise > baseline_wander * 0.8) {
    quality_grade <- "moderate"
  }

  list(
    quality_grade = quality_grade,
    usable_fraction = usable_fraction,
    baseline_wander = baseline_wander,
    hf_noise = hf_noise
  )
}

compute_qrs_features <- function(ecg_df) {

  fs <- attr(ecg_df, "fs")
  lead <- attr(ecg_df, "leads")[1]
  signal <- ecg_df[[lead]]

  empty_result <- function(status = NA_character_) {
    list(
      qrs_duration_median = NA_real_,
      qrs_duration_sd = NA_real_,
      qrs_wide_fraction = NA_real_,
      morphology_stability = status
    )
  }

  if (is.null(fs) || is.null(signal)) return(empty_result())
  if (all(is.na(signal))) return(empty_result("unavailable"))

  hrv <- compute_hrv(ecg_df)
  beat_times <- hrv$rr_intervals$time_sec
  if (length(beat_times) == 0) return(empty_result("no beats detected"))

  approx_idx <- round(beat_times * fs) + 1
  approx_idx <- approx_idx[approx_idx > 1 & approx_idx < length(signal)]

  if (length(approx_idx) < 3) return(empty_result("too few beats"))

  qrs_durations_ms <- c()

  for (ai in approx_idx) {

    refine_left  <- max(1, ai - round(0.08 * fs))
    refine_right <- min(length(signal), ai + round(0.08 * fs))
    local_seg <- signal[refine_left:refine_right]

    if (any(is.na(local_seg))) next

    local_baseline <- stats::median(local_seg)
    local_dev <- abs(local_seg - local_baseline)
    local_peak_pos <- which.max(local_dev)

    ri <- refine_left + local_peak_pos - 1
    peak_amp_signed <- signal[ri] - local_baseline
    peak_amp_abs <- abs(peak_amp_signed)

    if (is.na(peak_amp_abs) || peak_amp_abs < 1e-6) next

    left_bound  <- max(1, ri - round(0.15 * fs))
    right_bound <- min(length(signal), ri + round(0.15 * fs))

    thr_abs <- 0.30 * peak_amp_abs

    li <- ri
    while (li > left_bound &&
           abs(signal[li] - local_baseline) > thr_abs) {
      li <- li - 1
    }

    rj <- ri
    while (rj < right_bound &&
           abs(signal[rj] - local_baseline) > thr_abs) {
      rj <- rj + 1
    }

    dur_ms <- (rj - li) / fs * 1000


    if (!is.na(dur_ms) && dur_ms >= 20 && dur_ms <= 150) {
      qrs_durations_ms <- c(qrs_durations_ms, dur_ms)
    }
  }

  if (length(qrs_durations_ms) < 3) {
    return(empty_result("estimation unstable"))
  }

  qrs_duration_median <- stats::median(qrs_durations_ms)
  qrs_duration_sd <- stats::sd(qrs_durations_ms)
  qrs_wide_fraction <- mean(qrs_durations_ms > 120)

  morphology_stability <- if (is.na(qrs_duration_sd)) {
    "estimation unstable"
  } else if (qrs_duration_sd < 10) {
    "stable"
  } else if (qrs_duration_sd < 20) {
    "moderately variable"
  } else {
    "variable"
  }

  list(
    qrs_duration_median = qrs_duration_median,
    qrs_duration_sd = qrs_duration_sd,
    qrs_wide_fraction = qrs_wide_fraction,
    morphology_stability = morphology_stability
  )
}



compute_pwave_features <- function(ecg_df) {
  list(
    p_detectability = NA_real_,
    p_before_qrs_fraction = NA_real_,
    p_morphology_consistency = NA_character_
  )
}

compute_st_features <- function(ecg_df) {
  list(
    st_offset_median = NA_real_,
    st_offset_abs_max = NA_real_,
    st_observation = NA_character_
  )
}

compute_qt_features <- function(ecg_df) {
  list(
    qt_median = NA_real_,
    qtc_bazett = NA_real_,
    qtc_fridericia = NA_real_,
    qt_confidence = NA_character_
  )
}
