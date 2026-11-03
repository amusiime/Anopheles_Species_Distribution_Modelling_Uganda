# Uganda *Anopheles gambiae* abundance: models and 1 km prediction maps

Negative-binomial models of *An. gambiae* counts from entomological surveillance,
with climate, land cover, human footprint and ITN use, and annual/monthly 1 km
prediction surfaces for Uganda.

## Run

Open the project root (the folder containing `.here`) in R >= 4.1, then:

```r
source("run_all.R")   # or run the numbered scripts in order
```

| Script | Purpose | Main output |
|---|---|---|
| `R/config.R` | Paths, constants, shared helpers (sourced by every script) | |
| `00_install_packages.R` | Install packages | |
| `01_clean_data.R` | Clean raw entomology records | `data/processed/ento_clean.csv` |
| `02_prepare_covariates.R` | WorldClim, land cover, human footprint on a 1 km grid | `data/processed/*.tif` |
| `02b_prepare_itn_raster.R` | Regional ITN use (2024) as a raster | `itn_use_prediction_1km.tif` |
| `03_prepare_model_data.R` | Extract covariates, offsets, ITN use | `model_data_{annual,monthly}_itn.csv` |
| `04_fit_models.R` | Fit M1-M4 (annual and monthly) | `outputs/models/` |
| `05_diagnostics.R` | DHARMa residual diagnostics | `outputs/tables/`, `outputs/figures/` |
| `06_spatial_prediction.R` | Predict M3 and M4 over Uganda | `outputs/predictions/` |
| `07_final_prediction_map.R` | Maps | `outputs/figures/` |

Models: M1 climate; M2 + land cover + human footprint; M3 + ITN use; M4 = M3 with
the continental model prediction added to the offset.

## Required raw inputs (not produced by the code)

Fill in the source, version, licence and citation before publishing.

| File | Description | Source |
|---|---|---|
| `data/raw/ento_august.csv` | DHIS2 export of entomology records (all months despite the name) | TODO |
| `data/raw/itn_data_clean.csv` | Regional ITN use by `scenario`, `admin_name`, `date`, `use` | TODO |
| `data/raw/Uganda Districts-wgs84.shp` | District boundaries (`dname2019`, `d`, `Region`) | TODO |
| `data/raw/continental/uganda_preds_p.tif` | Continental *An. gambiae* prediction (band 4) | TODO |
| `data/raw/environmental_space/uganda_environmental_space/` | `landcover_<class>_1992_2022.tif`, `footprint_2000_2024.tif` | TODO |

WorldClim 2.1 and GADM are downloaded by `geodata` in script 02.

## Assumptions to confirm

- ER02 GPS strings list longitude first, then latitude.
- `event_date` is day-month-year.
- Band 4 of the continental raster is *An. gambiae* (script 03 prints the band name).
- Household GPS coordinates are sensitive: do not publish `ento_clean.csv` or the model
  data without aggregating or jittering them.

## Software

Run `renv::init()` once and commit `renv.lock`; `run_all.R` writes
`outputs/session_info.txt`.
