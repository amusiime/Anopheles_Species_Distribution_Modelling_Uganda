# ============================================================
# 06_final_prediction_map.R
# Anopheles gambiae s.l. prediction map - Uganda
# July | 1-km nominal resolution
# ============================================================

library(terra)
library(sf)
library(tidyverse)
library(patchwork)
library(ggspatial)
library(scales)

# ------------------------------------------------------------
# 1. SETTINGS
# ------------------------------------------------------------

month <- "July"
out <- "outputs/figures"

dir.create(out, recursive = TRUE, showWarnings = FALSE)

m2_file   <- "outputs/predictions/an_gambiae_M2_1km.tif"
m3_file   <- "outputs/predictions/an_gambiae_M3_1km.tif"
diff_file <- "outputs/predictions/M3_minus_M2_1km.tif"

uganda_file <- "data/raw/Uganda Districts-wgs84.shp"
lakes_file  <- "data/raw/lakes_Clip1.shp"

pred_limit <- 10
diff_limit <- 10

# ------------------------------------------------------------
# 2. CHECK FILES
# ------------------------------------------------------------

files <- c(
  m2_file,
  m3_file,
  diff_file,
  uganda_file,
  lakes_file
)

missing <- files[!file.exists(files)]

if (length(missing) > 0) {
  stop(
    "Missing files:\n",
    paste(missing, collapse = "\n")
  )
}

# ------------------------------------------------------------
# 3. READ RASTERS
# ------------------------------------------------------------

m2  <- rast(m2_file)
m3  <- rast(m3_file)
dif <- rast(diff_file)

if (!compareGeom(
  m2,
  m3,
  dif,
  stopOnError = FALSE
)) {
  stop("Prediction rasters are not aligned.")
}

message("Prediction rasters loaded and aligned.")

# ------------------------------------------------------------
# 4. READ UGANDA
# ------------------------------------------------------------

uganda <- st_read(
  uganda_file,
  quiet = TRUE
)

uganda <- st_make_valid(uganda)

uganda <- uganda[
  !st_is_empty(uganda),
  ,
  drop = FALSE
]

uganda <- st_transform(
  uganda,
  st_crs(crs(m2))
)

# National boundary
uganda_boundary <- st_as_sf(
  st_union(uganda)
)

# Raster vector
uganda_vect <- vect(
  uganda_boundary
)

# ------------------------------------------------------------
# 5. READ LAKES
# ------------------------------------------------------------

lakes <- st_read(
  lakes_file,
  quiet = TRUE
)

lakes <- st_make_valid(lakes)

lakes <- lakes[
  !st_is_empty(lakes),
  ,
  drop = FALSE
]

lakes <- st_transform(
  lakes,
  st_crs(crs(m2))
)

# ------------------------------------------------------------
# 6. CROP AND MASK
# ------------------------------------------------------------

m2 <- mask(
  crop(m2, uganda_vect),
  uganda_vect
)

m3 <- mask(
  crop(m3, uganda_vect),
  uganda_vect
)

dif <- mask(
  crop(dif, uganda_vect),
  uganda_vect
)

# ------------------------------------------------------------
# 7. RASTER DATA FRAMES
# ------------------------------------------------------------

make_df <- function(r, name) {
  
  x <- as.data.frame(
    r,
    xy = TRUE,
    na.rm = FALSE
  )
  
  names(x)[3] <- name
  
  x[[name]][
    !is.finite(x[[name]])
  ] <- NA
  
  x
}

m2_df  <- make_df(m2, "prediction")
m3_df  <- make_df(m3, "prediction")
dif_df <- make_df(dif, "difference")

# ------------------------------------------------------------
# 8. DIFFERENCE DISPLAY
# ------------------------------------------------------------

dif_df$display <- sign(
  dif_df$difference
) * sqrt(
  abs(dif_df$difference)
)

# ------------------------------------------------------------
# 9. COLOUR PALETTES
# ------------------------------------------------------------

pred_cols <- c(
  "#440154",
  "#482878",
  "#3E4989",
  "#31688E",
  "#26828E",
  "#1F9E89",
  "#35B779",
  "#6CCE59",
  "#B4DE2C",
  "#FDE725"
)

diff_cols <- c(
  "#053061",
  "#2166AC",
  "#4393C3",
  "#92C5DE",
  "#D1E5F0",
  "#FFFFFF",
  "#FDDBC7",
  "#F4A582",
  "#D6604D",
  "#B2182B",
  "#67001F"
)

# ------------------------------------------------------------
# 10. PREDICTION SCALE
# ------------------------------------------------------------

pred_scale <- scale_fill_gradientn(
  colours = pred_cols,
  limits = c(0, pred_limit),
  breaks = seq(0, 10, 2),
  labels = seq(0, 10, 2),
  oob = squish,
  na.value = "transparent",
  name = "Predicted\nmosquitoes",
  guide = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = unit(5, "cm"),
    barheight = unit(0.35, "cm")
  )
)

# ------------------------------------------------------------
# 11. DIFFERENCE SCALE
# ------------------------------------------------------------

original_breaks <- seq(
  -diff_limit,
  diff_limit,
  2
)

display_breaks <- sign(
  original_breaks
) * sqrt(
  abs(original_breaks)
)

diff_scale <- scale_fill_gradientn(
  colours = diff_cols,
  limits = c(
    -sqrt(diff_limit),
    sqrt(diff_limit)
  ),
  breaks = display_breaks,
  labels = original_breaks,
  oob = squish,
  na.value = "transparent",
  name = "M3 − M2",
  guide = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = unit(5.5, "cm"),
    barheight = unit(0.35, "cm")
  )
)

# ------------------------------------------------------------
# 12. MAP LAYERS
# ------------------------------------------------------------

lake_layer <- geom_sf(
  data = lakes,
  inherit.aes = FALSE,
  fill = "#9EDDF5",
  colour = NA
)

district_layer <- geom_sf(
  data = uganda,
  inherit.aes = FALSE,
  fill = NA,
  colour = "grey65",
  linewidth = 0.10
)

border_layer <- geom_sf(
  data = uganda_boundary,
  inherit.aes = FALSE,
  fill = NA,
  colour = "black",
  linewidth = 0.70
)

# ------------------------------------------------------------
# 13. NORTH ARROW AND SCALE BAR
# ------------------------------------------------------------

north <- annotation_north_arrow(
  location = "tl",
  which_north = "true",
  pad_x = unit(0.12, "cm"),
  pad_y = unit(0.12, "cm"),
  height = unit(0.65, "cm"),
  width = unit(0.65, "cm"),
  style = north_arrow_orienteering(
    text_size = 6
  )
)

bar <- annotation_scale(
  location = "bl",
  width_hint = 0.20,
  text_cex = 0.60
)

# ------------------------------------------------------------
# 14. THEME
# ------------------------------------------------------------

theme_map <- theme_minimal(
  base_size = 10
) +
  theme(
    panel.grid.major = element_line(
      colour = "grey82",
      linewidth = 0.20
    ),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(
      colour = "grey25",
      size = 8
    ),
    panel.background = element_rect(
      fill = "#E6E6E6",
      colour = NA
    ),
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    panel.border = element_rect(
      fill = NA,
      colour = "grey50",
      linewidth = 0.30
    ),
    legend.position = "bottom",
    legend.title = element_text(
      face = "bold",
      size = 9
    ),
    legend.text = element_text(
      size = 8
    ),
    legend.key.width = unit(
      2.3,
      "cm"
    )
  )

# ------------------------------------------------------------
# 15. MODEL 2
# ------------------------------------------------------------

p_m2 <- ggplot() +
  
  geom_raster(
    data = m2_df,
    aes(
      x = x,
      y = y,
      fill = prediction
    )
  ) +
  
  lake_layer +
  district_layer +
  border_layer +
  
  pred_scale +
  
  labs(
    title = "A. Model 2 (M2)",
    subtitle = "Local + environmental predictors"
  ) +
  
  north +
  bar +
  
  coord_sf(expand = FALSE) +
  
  theme_map

# ------------------------------------------------------------
# 16. MODEL 3
# ------------------------------------------------------------

p_m3 <- ggplot() +
  
  geom_raster(
    data = m3_df,
    aes(
      x = x,
      y = y,
      fill = prediction
    )
  ) +
  
  lake_layer +
  district_layer +
  border_layer +
  
  pred_scale +
  
  labs(
    title = "B. Model 3 (M3)",
    subtitle = "Integrated model + continental offset"
  ) +
  
  north +
  bar +
  
  coord_sf(expand = FALSE) +
  
  theme_map

# ------------------------------------------------------------
# 17. DIFFERENCE
# ------------------------------------------------------------

p_diff <- ggplot() +
  
  geom_raster(
    data = dif_df,
    aes(
      x = x,
      y = y,
      fill = display
    )
  ) +
  
  lake_layer +
  district_layer +
  border_layer +
  
  diff_scale +
  
  labs(
    title = "C. Continental model effect",
    subtitle = "M3 − M2"
  ) +
  
  north +
  bar +
  
  coord_sf(expand = FALSE) +
  
  theme_map

# ------------------------------------------------------------
# 18. COMBINE
# ------------------------------------------------------------

final_map <-
  (p_m2 | p_m3 | p_diff) +
  
  plot_layout(
    widths = c(
      1,
      1,
      1.05
    ),
    guides = "collect"
  ) +
  
  plot_annotation(
    title =
      "Predicted Anopheles gambiae s.l. abundance in Uganda",
    
    subtitle = paste0(
      "Population-level predictions for ",
      month,
      " (1-km nominal resolution)"
    ),
    
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 18,
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = 10,
        hjust = 0.5
      )
    )
  )

final_map <- final_map &
  theme(
    legend.position = "bottom"
  )

# Display
print(final_map)

# ------------------------------------------------------------
# 19. OUTPUT NAMES
# ------------------------------------------------------------

main_png <- file.path(
  out,
  "final_an_gambiae_prediction_map_July_1km.png"
)

main_pdf <- file.path(
  out,
  "final_an_gambiae_prediction_map_July_1km.pdf"
)

m2_png <- file.path(
  out,
  "an_gambiae_M2_final_1km.png"
)

m3_png <- file.path(
  out,
  "an_gambiae_M3_final_1km.png"
)

diff_png <- file.path(
  out,
  "an_gambiae_M3_minus_M2_final_1km.png"
)

# ------------------------------------------------------------
# 20. SAVE FINAL PNG
# ------------------------------------------------------------

ggsave(
  filename = main_png,
  plot = final_map,
  width = 18,
  height = 8,
  units = "in",
  dpi = 600,
  bg = "white"
)

# ------------------------------------------------------------
# 21. SAVE FINAL PDF
# ------------------------------------------------------------

pdf_out <- main_pdf

if (file.exists(pdf_out)) {
  
  removed <- try(
    file.remove(pdf_out),
    silent = TRUE
  )
  
  if (identical(removed, FALSE)) {
    
    pdf_out <- file.path(
      out,
      paste0(
        "final_an_gambiae_prediction_map_July_1km_",
        format(
          Sys.time(),
          "%Y%m%d_%H%M%S"
        ),
        ".pdf"
      )
    )
    
    message(
      "Existing PDF is locked. Saving: ",
      pdf_out
    )
  }
}

ggsave(
  filename = pdf_out,
  plot = final_map,
  width = 18,
  height = 8,
  units = "in",
  device = grDevices::pdf,
  bg = "white"
)

# ------------------------------------------------------------
# 22. SAVE INDIVIDUAL MAPS
# ------------------------------------------------------------

ggsave(
  filename = m2_png,
  plot = p_m2,
  width = 7,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = m3_png,
  plot = p_m3,
  width = 7,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = diff_png,
  plot = p_diff,
  width = 7,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "white"
)

# ------------------------------------------------------------
# 23. SAVE SETTINGS
# ------------------------------------------------------------

write_csv(
  tibble(
    target = "Anopheles gambiae s.l. abundance",
    month = month,
    resolution = "1-km nominal",
    prediction_scale = "0 to 10",
    difference_scale = "-10 to +10",
    m2_raster = m2_file,
    m3_raster = m3_file,
    difference_raster = diff_file,
    main_png = main_png,
    main_pdf = pdf_out
  ),
  file.path(
    out,
    "final_map_settings.csv"
  )
)

# ------------------------------------------------------------
# 24. CHECK OUTPUTS
# ------------------------------------------------------------

outputs <- c(
  main_png,
  pdf_out,
  m2_png,
  m3_png,
  diff_png
)

if (any(!file.exists(outputs))) {
  
  stop(
    "Some output files were not created:\n",
    paste(
      outputs[!file.exists(outputs)],
      collapse = "\n"
    )
  )
}

message("")
message("==============================================")
message("FINAL MAP CREATED SUCCESSFULLY")
message("==============================================")
message("Prediction scale: 0 to 10")
message("Difference scale: -10 to +10")
message("PNG: ", main_png)
message("PDF: ", pdf_out)
message("==============================================")