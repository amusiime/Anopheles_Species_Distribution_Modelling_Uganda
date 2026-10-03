
# ============================================================
# 07_final_prediction_map.R
# Anopheles gambiae abundance maps - Uganda
# ============================================================

suppressPackageStartupMessages({
  library(terra)
  library(ggplot2)
  library(viridis)
  library(patchwork)
})

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. COLOUR SCALE
# ------------------------------------------------------------

get_upper_limit <- function(r) {
  x <- as.numeric(terra::values(r))
  quantile(x, 0.99, na.rm = TRUE)
}

# ------------------------------------------------------------
# 2. MAP FUNCTION
# ------------------------------------------------------------

create_map <- function(r, title, upper, compact = FALSE) {
  
  r <- project(r, "EPSG:4326", method = "bilinear")
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "abundance"
  
  ggplot(df, aes(x, y, fill = abundance)) +
    geom_raster() +
    scale_fill_viridis_c(
      option = "magma",
      limits = c(0, upper),
      oob = scales::squish,
      name = "Predicted\nabundance"
    ) +
    coord_equal(expand = FALSE) +
    labs(
      title = title,
      x = if (compact) NULL else "Longitude",
      y = if (compact) NULL else "Latitude"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = if (compact) 11 else 15,
        hjust = 0.5
      ),
      panel.grid = element_blank(),
      axis.text = if (compact) element_blank() else element_text(),
      axis.ticks = if (compact) element_blank() else element_line(),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      plot.margin = margin(3, 3, 3,3)
    )
}

# ------------------------------------------------------------
# 3. ANNUAL PREDICTION MAP
# ------------------------------------------------------------

M1 <- rast("outputs/predictions/M1_annual_1km.tif")

map_M1 <- create_map(
  M1,
  "Annual Predicted Anopheles gambiae Abundance",
  get_upper_limit(M1)
)

ggsave(
  "outputs/figures/M1_annual_prediction_map.png",
  map_M1, width = 9, height = 10, dpi = 400
)


# ------------------------------------------------------------
# 4. COMBINED MONTHLY PREDICTION MAP
# ------------------------------------------------------------


M2 <- rast("outputs/predictions/M2_monthly_1km.tif")

months <- month.abb
M2_limit <- get_upper_limit(M2)

monthly_maps <- lapply(seq_len(12), function(m) {
  
  create_map(
    M2[[m]],
    months[m],
    M2_limit,
    compact = TRUE
  )
})

monthly_stack <- wrap_plots(
  monthly_maps,
  ncol = 4,
  guides = "collect"
) +
  
  theme(
    legend.position = "right",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

print(monthly_stack)

ggsave(
  "outputs/figures/M2_monthly_prediction_stack.png",
  monthly_stack, width = 16, height = 20,
  dpi = 400, bg = "white"
)





  