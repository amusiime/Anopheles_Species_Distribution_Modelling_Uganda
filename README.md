# Spatial Modelling of Malaria Vector Abundance in Uganda

## Purpose

This repository implements the revised Objective 1 workflow for modelling *Anopheles gambiae* s.l. abundance in Uganda using national entomological surveillance data, environmental predictors, repeated-site structure, sampling intensity, temporal trend, travel accessibility, and a validated continental ecological prediction surface.

The workflow uses **three nested negative-binomial abundance models** and produces standardized 1-km national prediction surfaces.

## Three-model framework

| Model | Specification | Purpose |
|---|---|---|
| **M1** | Household + season + travel + sampling intensity + temporal trend | Local/household baseline |
| **M2** | M1 + environmental variables + site random effect | Primary environmental repeated-site model |
| **M3** | M2 + continental ecological offset | Integrated national/continental model |

### M1

```r
an_gambiae_total ~
  people_under_net + n_nets + sprayed + house_type + month +
  travel + log_effort + year_c
```

### M2

```r
an_gambiae_total ~
  people_under_net + n_nets + sprayed + house_type + month +
  travel + log_effort + year_c +
  tmax + tmin + precip + (1 | site_id)
```

### M3

```r
an_gambiae_total ~
  people_under_net + n_nets + sprayed + house_type + month +
  travel + log_effort + year_c +
  tmax + tmin + precip +
  offset(log_continental_offset) + (1 | site_id)
```

All three models use a **negative-binomial 2** distribution to accommodate overdispersed abundance counts.

## Sampling intensity

`effort` is the number of observations within a site-month and is therefore treated as a sampling-intensity predictor rather than an automatic exposure offset. The model variable is:

```r
log_effort = log1p(effort)
```

It is included in **M1**, and therefore retained in M2 and M3.

## Repeated observations

Repeated observations from the same surveillance site are represented explicitly in M2 and M3 using:

```r
(1 | site_id)
```

This accounts for correlation among repeated observations collected at the same surveillance site.

## Continental offset for M3

M3 requires a validated continental prediction raster:

```text
data/raw/continental/uganda_preds_p.tif
```

The current project is configured to use **band 4** as the *An. gambiae* s.l. continental prediction surface. This band assignment must be confirmed against the original Vector Atlas/source metadata before scientific interpretation.

The raster is converted to:

```r
log_continental_offset = log(continental_gambiae)
```

Values that are zero, negative, non-finite, or otherwise unusable are set to missing rather than artificially replaced.

**The continental raster is not fabricated or substituted with another raster.** If it is absent, the pipeline stops at M3 and clearly reports the required input.

## Coordinate quality

The cleaning stage retains observations with missing or invalid coordinates in the master cleaned dataset. Original coordinates are preserved in `longitude_original` and `latitude_original`; invalid working coordinates are set to `NA`. Only observations with valid working coordinates are used for spatial covariate extraction and spatial prediction.

## Environmental data

The supplied approximately 30-arc-second (~1 km) raster is read from:

```text
data/raw/covariates2(1).tif
```

The first three layers are named:

- `tmax`
- `tmin`
- `precip`

## Spatial prediction

The national prediction grid follows the environmental raster. Environmental predictors vary spatially. Household/intervention predictors are held at representative observed values to create a standardized national surface and are not interpreted as directly measured household conditions at every pixel.

For M3, the continental Gambiae surface is explicitly projected/resampled to the environmental prediction grid before prediction.

The standard prediction month is **July** and can be changed in `R/06_spatial_prediction.R`.

## Diagnostics

`R/05_diagnostics.R` runs DHARMa diagnostics for **all three models**, including:

- residual uniformity;
- dispersion;
- zero inflation;
- spatial autocorrelation;
- temporal autocorrelation.

Final interpretation and mapping should be based on model adequacy, not AIC alone.

## Workflow

```text
01_clean_data.R
        |
        v
01b_data_QA.R
        |
        v
04_prepare_travel.R
        |
        v
02_prepare_covariates.R
        |
        v
03_fit_models.R
        |
        v
05_diagnostics.R
        |
        v
06_spatial_prediction.R
```

## Run

Open `Spatial_Vector_Abundance_Uganda.Rproj` in RStudio and run:

```r
source("R/00_install_packages.R")
source("run_all.R")
```

If the travel-time raster is not already present, `R/04_prepare_travel.R` requires internet access to obtain the travel-time data.

Before running the full workflow, place the validated continental raster at:

```text
data/raw/continental/uganda_preds_p.tif
```

You can inspect its metadata with:

```r
source("R/07_integrated_continental_optional.R")
```

## Main outputs

### Cleaned data

- `data/processed/ento_clean.csv`
- `data/processed/model_data_environment.csv`
- `data/processed/environment_1km.tif`

### QA

- `outputs/qa/cleaning_summary.csv`
- `outputs/qa/coordinate_status.csv`
- `outputs/qa/invalid_coordinates.csv`
- `outputs/qa/missing_coordinates.csv`
- `outputs/qa/extreme_gambiae_counts.csv`
- `outputs/qa/species_missingness.csv`
- `outputs/qa/collection_method_summary.csv`
- `outputs/qa/site_month_summary.csv`
- `outputs/qa/sampling_effort_summary.csv`

### Models

- `outputs/models/vector_abundance_models.rds`
- `outputs/models/modeling_dataset.rds`
- `outputs/tables/model_comparison.csv`
- `outputs/tables/model_performance.csv`
- `outputs/tables/model_predictions.csv`

### Diagnostics

- `outputs/tables/DHARMa_summary.csv`
- `outputs/tables/model_residuals_all.csv`
- `outputs/tables/site_residual_summary.csv`
- `outputs/figures/DHARMa_M1.png`
- `outputs/figures/DHARMa_M2.png`
- `outputs/figures/DHARMa_M3.png`

### Spatial prediction

- `outputs/predictions/an_gambiae_M1_1km.tif`
- `outputs/predictions/an_gambiae_M2_1km.tif`
- `outputs/predictions/an_gambiae_M3_1km.tif`
- `outputs/predictions/M2_minus_M1_1km.tif`
- `outputs/predictions/M3_minus_M2_1km.tif`
- `outputs/figures/an_gambiae_M2_1km.png`

## Project structure

```text
Anopheles_Species_Distribution_Modelling_Uganda/
├── R/
│   ├── 00_install_packages.R
│   ├── 01_clean_data.R
│   ├── 01b_data_QA.R
│   ├── 02_prepare_covariates.R
│   ├── 03_fit_models.R
│   ├── 04_prepare_travel.R
│   ├── 05_diagnostics.R
│   ├── 06_spatial_prediction.R
│   └── 07_integrated_continental_optional.R
├── functions/
│   └── helpers.R
├── data/
│   ├── raw/
│   │   └── continental/
│   └── processed/
├── outputs/
├── archive/
├── run_all.R
├── README.md
└── Spatial_Vector_Abundance_Uganda.Rproj
```

## Data protection

The raw entomological dataset may contain restricted surveillance information. Do not push identifiable or restricted raw data to a public GitHub repository unless the applicable data-sharing permissions allow it.
