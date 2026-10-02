# ============================================================
# 08_integrated_continental_optional.R
# CONTINENTAL INPUT VALIDATION / METADATA CHECK
# ============================================================
#
# PURPOSE
# Validate the continental raster used as the ecological
# offset in Model 3 (M3).
#
# IMPORTANT
# Band 4 of uganda_preds_p.tif is configured as the
# Anopheles gambiae continental prediction surface.
#
# This script DOES NOT modify the raster or fit a model.
# It only checks that the expected raster and band exist
# and reports basic raster metadata.
#
# M3 uses the processed log-transformed offset created by:
#     R/02_prepare_spatial_covariates.R
#
# ============================================================

library(terra)

# ------------------------------------------------------------
# 1. INPUT SETTINGS
# ------------------------------------------------------------

rfile <- "data/raw/continental/uganda_preds_p.tif"

# Confirmed An. gambiae band
gambiae_band <- 4

# ------------------------------------------------------------
# 2. CHECK FILE EXISTS
# ------------------------------------------------------------

if (!file.exists(rfile)) {
  stop(
    "Missing validated continental raster: ",
    rfile
  )
}

# ------------------------------------------------------------
# 3. LOAD RASTER
# ------------------------------------------------------------

r <- terra::rast(rfile)

# ------------------------------------------------------------
# 4. CHECK CONFIGURED BAND
# ------------------------------------------------------------

if (gambiae_band > terra::nlyr(r)) {
  stop(
    "Configured An. gambiae band does not exist. ",
    "Requested band: ", gambiae_band,
    "; available bands: ", terra::nlyr(r)
  )
}

# ------------------------------------------------------------
# 5. REPORT RASTER METADATA
# ------------------------------------------------------------

cat(
  "\n============================================================",
  "\nCONTINENTAL RASTER VALIDATION",
  "\n============================================================",
  "\nRaster: ",
  rfile,
  "\nNumber of bands: ",
  terra::nlyr(r),
  "\nConfigured An. gambiae band: ",
  gambiae_band,
  "\nResolution: ",
  paste(terra::res(r), collapse = " x "),
  "\nCRS: ",
  terra::crs(r),
  "\nExtent: ",
  paste(terra::ext(r), collapse = " | "),
  "\n",
  sep = ""
)

# ------------------------------------------------------------
# 6. CHECK GAMBIae BAND VALUES
# ------------------------------------------------------------

gambiae <- r[[gambiae_band]]

gambiae_values <- terra::values(
  gambiae,
  mat = FALSE
)

cat(
  "\nAn. gambiae band summary:",
  "\n  Valid cells: ",
  sum(is.finite(gambiae_values)),
  "\n  Minimum: ",
  min(gambiae_values, na.rm = TRUE),
  "\n  Maximum: ",
  max(gambiae_values, na.rm = TRUE),
  "\n  Mean: ",
  mean(gambiae_values, na.rm = TRUE),
  "\n  NA cells: ",
  sum(is.na(gambiae_values)),
  "\n",
  sep = ""
)

# ------------------------------------------------------------
# 7. FINAL VALIDATION MESSAGE
# ------------------------------------------------------------

cat(
  "\n============================================================",
  "\nVALIDATION NOTE",
  "\n============================================================",
  "\nBand ",
  gambiae_band,
  " is configured as the continental An. gambiae",
  "\nprediction surface used for the M3 ecological offset.",
  "\n",
  "\nBefore final interpretation of M3, confirm from the",
  "\nsource metadata/documentation:",
  "\n  1. What Band 4 represents",
  "\n  2. The units/scale of Band 4",
  "\n  3. Whether values represent abundance, suitability,",
  "\n     probability, or another prediction quantity",
  "\n  4. The geographic and temporal reference of the surface",
  "\n",
  "\nNo raster values are modified by this script.",
  "\nThe processed M3 offset is generated separately by",
  "\nR/02_prepare_spatial_covariates.R.",
  "\n",
  sep = ""
)

# ============================================================
# END OF SCRIPT
# ============================================================
