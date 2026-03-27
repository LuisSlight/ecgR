# ecgR

**ecgR** is an R package and Shiny application for electrocardiogram (ECG) analysis.  
It supports waveform visualization, heart rate variability (HRV) analysis, structured ECG feature extraction, and automated natural-language interpretation.

## Features

- Read ECG signals from **MIT-BIH records**
- Support **CSV-based ECG input**
- Visualize **ECG waveforms** and **RR intervals**
- Compute HRV metrics:
  - Mean heart rate
  - SDNN
  - RMSSD
  - pNN50
- Extract ECG features:
  - Signal quality
  - QRS morphology stability
- Generate automated **natural-language ECG summaries**
- Explore results through an interactive **Shiny dashboard**

## Project structure

- `read_ecg()` — read MIT-BIH ECG records
- `read_ecg_csv()` — read ECG signals from CSV
- `compute_hrv()` — compute HRV metrics
- `extract_ecg_features()` — build a structured ECG feature object
- `interpret_ecg_features()` — generate human-readable ECG interpretations
- `launch_app()` — launch the Shiny ECG dashboard

## Example workflow

```r
devtools::load_all()

df <- read_ecg("path/to/record")
features <- extract_ecg_features(df)
interpret_ecg_features(features)

launch_app()
