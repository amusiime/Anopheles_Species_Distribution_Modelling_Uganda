
# ============================================================
# NATIONAL SPATIAL PREDICTION - AN. GAMBIAE, UGANDA
# Model M1: Negative Binomial
# ============================================================

# 1. PACKAGES ------------------------------------------------

library(tidyverse)
library(terra)
library(MASS)
library(sf)
library(ggplot2)
library(viridis)
library(scales)

# 2. FILE PATHS ----------------------------------------------

data_file <- "data/processed/model_data_observations.csv"
env_file <- "data/processed/environment_1km.tif"
travel_file <- "data/processed/travel_accessibility_1km.tif"
district_file <- "data/raw/Uganda Districts-wgs84.shp"

dir.create("outputs/models", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/predictions", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# 3. PREPARE MODELLING DATA ----------------------------------

d <- read_csv(data_file, show_col_types = FALSE)

model_data <- d %>%
  drop_na(
    an_gambiae_total,
    cell_number,
    tmin, tmax, precip, travel
  ) %>%
  group_by(cell_number) %>%
  summarise(
    an_gambiae_total = sum(an_gambiae_total),
    effort = n(),
    tmin = mean(tmin),
    tmax = mean(tmax),
    precip = mean(precip),
    travel = mean(travel),
    .groups = "drop"
  ) %>%
  mutate(log_effort = log(effort))

write_csv(
  model_data,
  "data/processed/model_data_cell_totals.csv"
)

# 4. FIT NEGATIVE BINOMIAL MODEL M1 --------------------------

M1 <- MASS::glm.nb(
  an_gambiae_total ~
    tmin + tmax + precip + travel +
    offset(log_effort),
  data = model_data
)

print(summary(M1))

saveRDS(M1, "outputs/models/M1.rds")

# 5. PREPARE ENVIRONMENTAL RASTERS ---------------------------

env <- rast(env_file)

# BIOCLIM bands: 6, 5 and 12

env <- env[[c(3, 2, 4)]]

names(env) <- c("tmin", "tmax", "precip")

template <- env[[1]]

# 6. PREPARE TRAVEL ACCESSIBILITY ----------------------------

travel <- rast(travel_file)

travel <- project(
  travel,
  template,
  method = "bilinear"
)

names(travel) <- "travel"

# 7. REFERENCE SAMPLING EFFORT -------------------------------

# log(1) = 0
log_effort <- init(template, 0)

names(log_effort) <- "log_effort"

# 8. BUILD PREDICTION STACK ----------------------------------

predictors <- c(
  env,
  travel,
  log_effort
)

names(predictors) <- c(
  "tmin",
  "tmax",
  "precip",
  "travel",
  "log_effort"
)

# 9. GENERATE NATIONAL SPATIAL PREDICTION --------------------

prediction <- terra::predict(
  predictors,
  M1,
  type = "response",
  na.rm = TRUE,
  filename = "outputs/predictions/an_gambiae_M1_1km.tif",
  overwrite = TRUE
)

names(prediction) <- "predicted_abundance"


# 10. PREPARE MAP DATA ---------------------------------------

pred_df <- as.data.frame(
  prediction,
  xy = TRUE,
  na.rm = TRUE
)

names(pred_df) <- c(
  "longitude",
  "latitude",
  "predicted_abundance"
)

# Load Uganda district boundaries
districts <- st_read(district_file, quiet = TRUE) %>%
  st_transform(4326)

# 11. CREATE GGPLOT MAP --------------------------------------

upper_limit <- as.numeric(
  quantile(
    pred_df$predicted_abundance,
    0.99,
    na.rm = TRUE
  )
)

p <- ggplot() +
  geom_raster(
    data = pred_df,
    aes(longitude, latitude, fill = predicted_abundance)
  ) +
  geom_sf(
    data = districts,
    fill = NA,
    colour = "grey65",
    linewidth = 0.15
  ) +
  scale_fill_viridis_c(
    option = "magma",
    trans = "sqrt",
    limits = c(0, upper_limit),
    oob = scales::squish,
    name = "Predicted\nabundance"
  ) +
  coord_sf(
    xlim = c(29.5, 35.1),
    ylim = c(-1.5, 4.3),
    expand = FALSE
  ) +
  labs(
    title = expression(
      paste("Predicted ", italic("Anopheles gambiae"), " abundance")
    ),
    subtitle = "Uganda | Negative binomial model M1",
    x = "Longitude",
    y = "Latitude",
    caption = "Predictions at reference sampling effort of one observation."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid = element_line(colour = "grey90", linewidth = 0.2)
  )

print(p)


# 1. PREPARE MONTHLY MODELLING DATA
# 1. Extract month from sampling date
d <- d %>%
  mutate(
    date = lubridate::ymd(event_date),
    month = lubridate::month(date)
  )
model_data_month <- d %>%
  tidyr::drop_na(
    an_gambiae_total,
    cell_number,
    month,
    tmin, tmax, precip, travel
  ) %>%
  group_by(cell_number, month) %>%
  summarise(
    an_gambiae_total = sum(an_gambiae_total),
    effort = n(),
    tmin = mean(tmin),
    tmax = mean(tmax),
    precip = mean(precip),
    travel = mean(travel),
    .groups = "drop"
  ) %>%
  mutate(
    month = factor(month, levels = 1:12),
    log_effort = log(effort)
  )

readr::write_csv(
  model_data_month,
  "data/processed/model_data_month_totals.csv"
)

M1_month <- MASS::glm.nb(
  an_gambiae_total ~
    tmin + tmax + precip + travel +
    month +
    offset(log_effort),
  data = model_data_month
)

summary(M1_month)

saveRDS(
  M1_month,
  "outputs/models/M1_month.rds"
)


# 5. PREPARE RASTERS -----------------------------------------

env <- rast(env_file)[[c(3, 2, 4)]]
names(env) <- c("tmin", "tmax", "precip")

template <- env[[1]]

travel <- project(
  rast(travel_file),
  template,
  method = "bilinear"
)
names(travel) <- "travel"

log_effort <- init(template, 0)
names(log_effort) <- "log_effort"




# 6. GENERATE MONTHLY PREDICTIONS ----------------------------

month_names <- month.name
pred_data <- list()

for (m in 1:12) {
  
  predictors <- c(env, travel, init(template, m), log_effort)
  names(predictors) <- c(
    "tmin", "tmax", "precip", "travel", "month", "log_effort"
  )
  
  prediction <- terra::predict(
    predictors, M1_month,
    fun = function(model, data, ...) {
      data$month <- factor(
        as.character(as.integer(data$month)),
        levels = model$xlevels[["month"]]
      )
      predict(model, newdata = data, type = "response")
    },
    na.rm = TRUE,
    filename = sprintf(
      "outputs/predictions/an_gambiae_M1_%s.tif",
      tolower(month_names[m])
    ),
    overwrite = TRUE
  )
  
  pred_df <- as.data.frame(prediction, xy = TRUE, na.rm = TRUE)
  names(pred_df) <- c("longitude", "latitude", "predicted_abundance")
  pred_df$month <- month_names[m]
  pred_data[[m]] <- pred_df
}

all_pred_df <- bind_rows(pred_data)


# 7. PREPARE DISTRICTS AND MAP SCALE -------------------------

districts <- st_read(district_file, quiet = TRUE) %>%
  st_transform(4326)

upper_limit <- quantile(
  all_pred_df$predicted_abundance, 0.99, na.rm = TRUE
)


# 8. CREATE 12 MONTHLY MAPS ----------------------------------

library(patchwork)
library(scales)

monthly_maps <- list()

for (m in month_names) {
  
  monthly_maps[[m]] <- ggplot(
    filter(all_pred_df, month == m)
  ) +
    geom_raster(aes(longitude, latitude, fill = predicted_abundance)) +
    
    scale_fill_viridis_c(
      option = "magma", trans = "sqrt",
      limits = c(0, upper_limit),
      oob = scales::squish,
      name = "Predicted\nabundance"
    ) +
    theme_void()}


# 9. COMBINE, DISPLAY AND SAVE ----------

monthly_panel <- wrap_plots(
  monthly_maps, ncol = 4, guides = "collect"
) +
  plot_annotation(
    title = expression(
      paste("Monthly Predicted ", italic("Anopheles gambiae"), " Abundance")
    )
  ) &
  theme(legend.position = "right")

print(monthly_panel)

ggsave(
  "outputs/figures/an_gambiae_M1_12_monthly_maps.png",
  monthly_panel, width = 16, height = 12, dpi = 600, bg = "white"
)

ggsave(
  "outputs/figures/an_gambiae_M1_12_monthly_maps.pdf",
  monthly_panel, width = 16, height = 12
)







