
# 07_final_prediction_map.R
# Map annual predictions and monthly prediction panels

library(terra)
library(ggplot2)
library(patchwork)
library(scales)

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# ---- Prediction files -------------------------------------------------------
annual_files <- c(
  annual_M1 = "outputs/predictions/annual_M1.tif",
  annual_M2 = "outputs/predictions/annual_M2.tif"
)

monthly_files <- list(
  M1 = sprintf("outputs/predictions/monthly_M1_%d.tif", 1:12),
  M2 = sprintf("outputs/predictions/monthly_M2_%d.tif", 1:12)
)

# ---- Check files ------------------------------------------------------------
all_files <- c(annual_files, unlist(monthly_files))
missing <- all_files[!file.exists(all_files)]

if (length(missing)) {
  stop("Missing prediction files: ", paste(missing, collapse = ", "))
}

# ---- Common Uganda raster extent -------------------------------------------
template <- rast(monthly_files$M1[1])[[1]]
e <- ext(template)
x_limits <- c(xmin(e), xmax(e))
y_limits <- c(ymin(e), ymax(e))

# Warn if any file has a different grid from the template
for (f in all_files) {
  if (!compareGeom(template, rast(f)[[1]], stopOnError = FALSE)) {
    warning("Geometry differs from template: ", f)
  }
}

# ---- Helper: shared colour limits ------------------------------------------
# q = 1 uses the true max; q < 1 clamps the upper limit (robust to outliers)
get_limits <- function(files, q = 1) {
  rng <- sapply(files, function(f) {
    v <- values(rast(f)[[1]], na.rm = TRUE)
    c(min(v), quantile(v, q))
  })
  c(min(rng[1, ]), max(rng[2, ]))
}

# ---- Plot function ----------------------------------------------------------
plot_raster <- function(file, title, limits = NULL) {
  
  r <- rast(file)[[1]]
  dat <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(dat)[3] <- "prediction"
  
  ggplot(dat, aes(x = x, y = y, fill = prediction)) +
    geom_raster() +
    coord_equal(
      xlim = x_limits,
      ylim = y_limits,
      expand = FALSE
    ) +
    scale_fill_viridis_c(
      name = "Abundance",
      limits = limits,
      oob = scales::squish,
      labels = label_number()
    ) +
    labs(title = title, x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text = element_text(size = 7),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7)
    )
}

# ---- Annual maps ------------------------------------------------------------
for (nm in names(annual_files)) {
  
  lim <- get_limits(annual_files[[nm]])
  p <- plot_raster(annual_files[[nm]], nm, lim)
  
  ggsave(
    sprintf("outputs/figures/%s.png", nm),
    p, width = 8, height = 6, dpi = 300
  )
}

# ---- Monthly maps -----------------------------------------------------------
for (nm in names(monthly_files)) {
  
  # Shared limits across all 12 months; 99th percentile avoids outlier washout
  lim <- get_limits(monthly_files[[nm]], q = 0.99)
  
  plots <- lapply(1:12, function(i) {
    plot_raster(monthly_files[[nm]][i], month.abb[i], lim) +
      theme(axis.title = element_blank())
  })
  
  monthly_map <- wrap_plots(plots, ncol = 4, nrow = 3, guides = "collect") +
    plot_annotation(
      title = paste("Monthly vector abundance predictions:", nm)
    ) &
    theme(legend.position = "right")
  
  ggsave(
    sprintf("outputs/figures/monthly_predictions_%s.png", nm),
    monthly_map,
    width = 16, height = 12, dpi = 200
  )
}

message("Prediction maps saved in outputs/figures/.")

