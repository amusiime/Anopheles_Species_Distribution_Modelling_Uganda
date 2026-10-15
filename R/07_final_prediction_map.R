
library(terra)
library(ggplot2)
library(patchwork)

# 1. Load prediction rasters
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

annual_files <- c(
  "outputs/predictions/annual_M1.tif",
  "outputs/predictions/annual_M2.tif"
)

monthly_files <- list(
  M1 = paste0("outputs/predictions/monthly_M1_", 1:12, ".tif"),
  M2 = paste0("outputs/predictions/monthly_M2_", 1:12, ".tif")
)

months <- month.abb

# 2. Plot annual predictions
for (f in annual_files) {
  r <- rast(f)
  p <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(p)[3] <- "prediction"
  
  g <- ggplot(p, aes(x, y, fill = prediction)) +
    geom_raster() +
    coord_equal() +
    scale_fill_viridis_c(name = "Predicted abundance") +
    labs(title = tools::file_path_sans_ext(basename(f)),
         x = "Longitude", y = "Latitude") +
    theme_minimal()
  
  ggsave(
    paste0("outputs/figures/",
           tools::file_path_sans_ext(basename(f)), ".png"),
    g, width = 8, height = 6, dpi = 300
  )
}

# 3. Plot monthly predictions in a 4 x 3 grid
for (model in names(monthly_files)) {
  plots <- lapply(1:12, function(i) {
    r <- rast(monthly_files[[model]][i])
    d <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
    names(d)[3] <- "prediction"
    
    ggplot(d, aes(x, y, fill = prediction)) +
      geom_raster() +
      coord_equal() +
      scale_fill_viridis_c(name = "Abundance") +
      labs(title = months[i], x = NULL, y = NULL) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 6),
        legend.position = "right"
      )
  })
  
  final_map <- wrap_plots(plots, ncol = 4, nrow = 3) +
    plot_annotation(title = paste("Monthly vector abundance predictions:", model))
  
  ggsave(
    paste0("outputs/figures/monthly_predictions_", model, ".png"),
    final_map, width = 16, height = 10, dpi = 300
  )
}



