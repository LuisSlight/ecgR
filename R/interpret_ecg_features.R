#' Interpret extracted ECG features
#'
#' @param features A list returned by extract_ecg_features()
#' @return A structured interpretation of the ECG features
#' @export
interpret_ecg_features <- function(features) {

  hr <- features$rate_rhythm$mean_hr
  sdnn <- features$rate_rhythm$sdnn
  rmssd <- features$rate_rhythm$rmssd
  qrs <- features$morphology$qrs
  beat_counts <- features$rate_rhythm$beat_counts
  signal_quality <- features$signal_quality

  details <- c()

  # ---- Heart rate ----
  hr_status <- if (hr < 60) {
    "low"
  } else if (hr > 100) {
    "high"
  } else {
    "normal"
  }

  if (hr_status == "low") {
    details <- c(details,
                 sprintf("Resting heart rate is %.1f bpm, which is below the typical adult resting range.", hr)
    )
  } else if (hr_status == "high") {
    details <- c(details,
                 sprintf("Resting heart rate is %.1f bpm, which is above the typical adult resting range.", hr)
    )
  } else {
    details <- c(details,
                 sprintf("Resting heart rate is %.1f bpm, which is within the normal resting range.", hr)
    )
  }

  # ---- HRV ----
  hrv_status <- if (!is.na(sdnn) && sdnn < 50) "reduced" else "healthy"

  if (hrv_status == "reduced") {
    details <- c(details,
                 sprintf("Overall HRV (SDNN: %.1f ms) appears somewhat reduced.", sdnn)
    )
  } else {
    details <- c(details,
                 sprintf("Overall HRV (SDNN: %.1f ms) appears within a healthy range.", sdnn)
    )
  }

  details <- c(details,
               sprintf("Short-term HRV (RMSSD: %.1f ms) reflects parasympathetic regulation.", rmssd)
  )

  # ---- QRS morphology ----
  qrs_status <- if (!is.null(qrs) && identical(qrs$morphology_stability, "stable")) {
    "stable"
  } else {
    "variable"
  }

  if (qrs_status == "stable") {
    details <- c(details,
                 "Estimated QRS morphology appears stable across detected beats."
    )
  } else {
    details <- c(details,
                 "QRS morphology shows some variability across detected beats."
    )
  }

  # ---- Irregular beats ----
  irregular_status <- "none_detected"

  if (!is.null(beat_counts) && length(beat_counts) > 0) {
    beat_vec <- as.integer(beat_counts)
    names(beat_vec) <- names(beat_counts)

    total_beats <- sum(beat_vec)
    normal_beats <- if ("N" %in% names(beat_vec)) beat_vec["N"] else 0
    irregular_beats <- total_beats - normal_beats

    normal_pct <- if (total_beats > 0) 100 * normal_beats / total_beats else NA_real_

    if (irregular_beats == 0) {
      details <- c(details,
                   sprintf("No significant irregular beats were detected. %.1f%% of beats were classified as normal.", normal_pct)
      )
      irregular_status <- "none_detected"
    } else if (irregular_beats <= 2) {
      details <- c(details,
                   sprintf("No significant irregular beats were detected in this short ECG segment (%.1f%% normal beats).", normal_pct)
      )
      irregular_status <- "none_detected"
    } else {
      details <- c(details,
                   sprintf("Multiple irregular beats were detected. %.1f%% of beats were classified as normal.", normal_pct)
      )
      irregular_status <- "present"
    }
  }

  # ---- Signal quality ----
  quality_status <- "unknown"

  if (!is.null(signal_quality) && !is.na(signal_quality$quality_grade)) {
    quality_status <- signal_quality$quality_grade

    if (quality_status == "good") {
      details <- c(details,
                   "Signal quality appears good, so feature estimates are likely reliable."
      )
    } else if (quality_status == "moderate") {
      details <- c(details,
                   "Signal quality is moderate; some feature estimates may be less reliable."
      )
    } else if (quality_status == "poor") {
      details <- c(details,
                   "Signal quality is poor, so waveform-based estimates should be interpreted with caution."
      )
    }
  }

  # ---- Overall summary ----
  if (hr_status == "normal" &&
      hrv_status == "healthy" &&
      qrs_status == "stable" &&
      irregular_status == "none_detected" &&
      quality_status == "good") {

    summary <- "Overall, this ECG segment is consistent with a normal heart rate pattern, healthy heart rate variability, stable QRS morphology, and no significant irregular beats."

  } else {
    summary <- "Overall, this ECG segment shows some deviations from the typical pattern and should be interpreted with caution."
  }

  list(
    summary = summary,
    details = details
  )
}
