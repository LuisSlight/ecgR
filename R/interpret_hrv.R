#' Generate a plain-English interpretation of HRV results
#'
#' @param hrv An \code{ecg_hrv} object from \code{compute_hrv()}.
#' @return A named list with interpretation text sections.
#' @export
interpret_hrv <- function(hrv) {

  sentences <- list()

  # ── Heart rate ─────────────────────────────────────────────────────────
  sentences$heart_rate <- if (hrv$mean_hr < 50) {
    list(level = "alert",
         text  = paste0("Resting heart rate is ", hrv$mean_hr,
                        " bpm, which is significantly below the normal range (60–100 bpm). ",
                        "This may indicate bradycardia and warrants medical attention."))
  } else if (hrv$mean_hr < 60) {
    list(level = "warning",
         text  = paste0("Resting heart rate is ", hrv$mean_hr,
                        " bpm, slightly below the typical range. ",
                        "This is common in athletes but may be worth discussing with a doctor."))
  } else if (hrv$mean_hr <= 100) {
    list(level = "normal",
         text  = paste0("Resting heart rate is ", hrv$mean_hr,
                        " bpm, which is within the normal range (60–100 bpm)."))
  } else {
    list(level = "alert",
         text  = paste0("Resting heart rate is ", hrv$mean_hr,
                        " bpm, which is above the normal range. ",
                        "Persistent elevated heart rate may indicate tachycardia."))
  }

  # ── SDNN ───────────────────────────────────────────────────────────────
  sentences$sdnn <- if (hrv$sdnn < 20) {
    list(level = "alert",
         text  = paste0("Overall heart rate variability (SDNN: ", hrv$sdnn,
                        " ms) is very low. Low HRV is associated with reduced autonomic ",
                        "flexibility and higher cardiovascular risk."))
  } else if (hrv$sdnn < 50) {
    list(level = "warning",
         text  = paste0("Overall HRV (SDNN: ", hrv$sdnn,
                        " ms) is slightly below the range typically seen in healthy adults (>50 ms). ",
                        "This may reflect mild stress, fatigue, or reduced recovery."))
  } else {
    list(level = "normal",
         text  = paste0("Overall HRV (SDNN: ", hrv$sdnn,
                        " ms) is within a healthy range, suggesting good autonomic function."))
  }

  # ── RMSSD ──────────────────────────────────────────────────────────────
  sentences$rmssd <- if (hrv$rmssd < 20) {
    list(level = "alert",
         text  = paste0("Short-term HRV (RMSSD: ", hrv$rmssd,
                        " ms) is very low, indicating reduced parasympathetic activity."))
  } else if (hrv$rmssd < 40) {
    list(level = "warning",
         text  = paste0("Short-term HRV (RMSSD: ", hrv$rmssd,
                        " ms) is below average. The parasympathetic nervous system ",
                        "may have reduced influence on heart rate regulation."))
  } else {
    list(level = "normal",
         text  = paste0("Short-term HRV (RMSSD: ", hrv$rmssd,
                        " ms) looks healthy, reflecting active parasympathetic regulation."))
  }

  # ── Beat composition ───────────────────────────────────────────────────
  counts     <- as.data.frame(hrv$beat_counts)
  colnames(counts) <- c("symbol", "count")
  total      <- sum(counts$count)
  n_normal   <- counts$count[counts$symbol == "N"]
  n_normal   <- if (length(n_normal) == 0) 0 else n_normal
  n_pvc      <- counts$count[counts$symbol == "V"]
  n_pvc      <- if (length(n_pvc) == 0) 0 else n_pvc
  n_pac      <- counts$count[counts$symbol == "A"]
  n_pac      <- if (length(n_pac) == 0) 0 else n_pac
  pct_normal <- round(100 * n_normal / total, 1)

  sentences$beats <- if (n_pvc > 10) {
    list(level = "alert",
         text  = paste0(n_pvc, " premature ventricular contractions (PVCs) were detected. ",
                        "Frequent PVCs (>10 per recording) may warrant further evaluation."))
  } else if (n_pac > 50) {
    list(level = "warning",
         text  = paste0(n_pac, " atrial premature beats were detected. ",
                        "Occasional premature beats are usually benign, but frequent ",
                        "episodes should be discussed with a healthcare provider."))
  } else if (n_pac > 0 || n_pvc > 0) {
    list(level = "warning",
         text  = paste0(
           if (n_pac > 0) paste0(n_pac, " atrial premature beat(s)") else NULL,
           if (n_pac > 0 && n_pvc > 0) " and " else NULL,
           if (n_pvc > 0) paste0(n_pvc, " ventricular premature beat(s)") else NULL,
           " were detected. Occasional premature beats are common and ",
           "usually benign in otherwise healthy individuals."))
  } else {
    list(level = "normal",
         text  = paste0("No significant irregular beats detected. ",
                        pct_normal, "% of beats were classified as normal."))
  }

  sentences
}
