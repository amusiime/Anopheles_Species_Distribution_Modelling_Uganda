#Packages

library(tidyverse)
library(terra)
library(geodata)
library(janitor)
library(DHARMa)
library(MASS)
library(tidyterra)
library(sf)
library(scales)
library(patchwork)
library(knitr)


conflicted::conflicts_prefer(
  dplyr::filter,
  dplyr::select,
  dplyr::lag
)

# Read in the dataset
ento_august <- read_csv(
  "ento_august.csv",
  col_types = cols(.default = col_character()),
  show_col_types = FALSE,
  trim_ws = TRUE
) |>
  clean_names()

###############################################################################
# Clean Entomology Dataset
###############################################################################

ento_clean1 <- ento_august %>%
  
  ###########################################################################
# Administrative hierarchy
###########################################################################
separate(
  organisation_unit_name_hierarchy,
  into = c(
    "owner", "region", "district",
    "district_lg", "sub_county",
    "health_facility"
  ),
  sep = " / ",
  remove = FALSE
) %>%
  mutate(
    district   = str_remove(district, " District$"),
    sub_county = str_remove(sub_county, " Subcounty$")
  ) %>%
  select(-owner, -district_lg) %>%
  
  ###########################################################################
# Remove DHIS2 prefix
###########################################################################
rename_with(~str_remove(.x, "^mal_001_")) %>%
  
  ###########################################################################
# Rename variables
###########################################################################
rename(
  
  # Household
  site_code        = er01_site_code,
  parish           = er03_parish,
  house_type       = er04_house_type,
  house_number     = er05_house_number,
  n_people         = er13_no_of_people_who_slept_in_the_house,
  
  # IRS
  sprayed          = er06_has_site_sprayed_in_the_past_12_months,
  insecticide      = er07_insecticide_sprayed,
  months_since_irs = er15_how_many_months_ago,
  
  # LLIN
  n_nets           = er16_number_of_lli_ns_available,
  llin_type        = er17_type_of_llin,
  llin_brand       = er28_brand_of_llin,
  people_under_net = er30_no_of_people_who_slept_under_llin,
  
  # Collection
  collection_method = er29_mosquito_collection_method_psc_ltc,
  
  # Other mosquitoes
  male_anopheles   = er25_male_anopheles,
  culex            = er22_culex,
  aedes            = er23_aedes,
  other_culicines  = er24_other_culicines
) %>%
  
  ###########################################################################
# Convert numeric variables
###########################################################################
mutate(
  across(
    c(
      longitude,
      latitude,
      n_people,
      months_since_irs,
      n_nets,
      people_under_net,
      male_anopheles,
      culex,
      aedes,
      other_culicines,
      starts_with("er18"),
      starts_with("er19"),
      starts_with("er20"),
      starts_with("er21")
    ),
    ~ suppressWarnings(as.numeric(.))
  )
) %>%
  
  ###########################################################################
# Calculate species totals
###########################################################################
mutate(
  
  an_gambiae_total =
    er18a_an_gambiae_s_l_fed +
    er18b_an_gambiae_s_l_unfed +
    er18c_an_gambiae_s_l_gravid +
    er18d_an_gambiae_s_l_half_gravid,
  
  an_funestus_total =
    er19a_an_funestus_s_l_fed +
    er19b_an_funestus_s_l_unfed +
    er19c_an_funestus_s_l_gravid +
    er19d_an_funestus_s_l_half_gravid,
  
  an_coustani_total =
    er20a_an_coustani_s_l_fed +
    er20b_an_coustani_s_l_unfed +
    er20c_an_coustani_s_l_gravid +
    er20d_an_coustani_s_l_half_gravid,
  
  other_anopheles_total =
    er21a_other_anopheles_fed +
    er21b_other_anopheles_unfed +
    er21c_other_anopheles_gravid +
    er21d_other_anopheles_half_gravid
) %>%
  
  ###########################################################################
# Keep variables
###########################################################################
select(
  
  # Time
  event,
  event_date,
  
  # Location
  region,
  district,
  sub_county,
  health_facility,
  longitude,
  latitude,
  
  # Household
  site_code,
  parish,
  house_type,
  house_number,
  n_people,
  
  # IRS
  sprayed,
  insecticide,
  months_since_irs,
  
  # LLIN
  n_nets,
  llin_type,
  llin_brand,
  people_under_net,
  
  # Collection
  collection_method,
  
  # Species totals
  an_gambiae_total,
  an_funestus_total,
  an_coustani_total,
  other_anopheles_total,
  
  # Other mosquitoes
  male_anopheles,
  culex,
  aedes,
  other_culicines
)




# Read shapefile
sub <- st_read("uganda_subcounties_2019-wgs84.shp", quiet = TRUE)

# Compute centroids
cent <- st_transform(sub, 32636)
cent <- st_centroid(cent)
cent <- st_transform(cent, 4326)

# Extract coordinates
xy <- st_coordinates(cent)

# Clean sub-county names
clean_subcounty <- function(x) {
  x <- str_to_title(x)
  x <- str_squish(x)
  x <- str_remove(x,
                  "\\s+(Town Council|Town Division|Municipal Division|Division|Subcounty)$")
  x
}

# Create lookup table
sub_coord <- data.frame(
  district = str_to_title(sub$District),
  sub_county = clean_subcounty(sub$sname2019),
  sub_long = xy[, 1],
  sub_lat = xy[, 2],
  stringsAsFactors = FALSE
)

# Remove duplicates
sub_coord <- unique(sub_coord)
###############################################################################
# Clean entomology dataset and fill missing coordinates
###############################################################################

ento_clean2 <- ento_clean1 |>
  select(
    event_date,
    district,
    sub_county,
    health_facility,
    longitude,
    latitude,
    house_type,
    house_number,
    sprayed,
    insecticide,
    n_nets,
    llin_brand,
    people_under_net,
    collection_method,
    an_gambiae_total,
    an_funestus_total,
    an_coustani_total,
    other_anopheles_total
  ) |>
  mutate(
    district   = str_to_title(district),
    sub_county = clean_subcounty(sub_county),
    across(c(longitude, latitude), as.numeric)
  ) |>
  left_join(
    sub_coord,
    by = c("district", "sub_county")
  ) |>
  mutate(
    longitude = coalesce(na_if(longitude, 0), sub_long),
    latitude  = coalesce(na_if(latitude, 0), sub_lat)
  ) |>
  select(-sub_long, -sub_lat)




# 4. DOWNLOAD AND PREPARE ENVIRONMENTAL VARIABLES


bioclim_uganda <- worldclim_country(
  country = "UGA",
  var = "bio",
  res = 0.5,
  path = getwd()
)


env_var <- bioclim_uganda[[c(
  5,
  6,
  12
)]]

names(env_var) <- c(
  "tmax",
  "tmin",
  "precip"
)


terra::writeRaster(
  env_var,
  "env_variables.tif",
  overwrite = TRUE
)

# ==================================
# 5. EXTRACT ENVIRONMENTAL AND TRAVEL VALUES
# ==================================

# Add unique ID
ento_data2 <- ento_clean2 |>
  mutate(ID = row_number())

# Convert household locations to spatial points
hh_points <- terra::vect(
  ento_data2,
  geom = c("longitude", "latitude"),
  crs = terra::crs(env_var)
)

# Extract environmental covariates
hh_env_values <- terra::extract(env_var, hh_points) |>
  as_tibble()

# Combine with household data
hh_env <- ento_data2 |>
  left_join(hh_env_values, by = "ID")

# Travel-time raster
# ============================================================
# Travel-time accessibility raster — Uganda
# ============================================================

# Uganda boundary
uganda <- gadm("UGA", level = 0, path = getwd())

# Create Uganda mask
uganda_mask <- mask(
  bioclim_uganda[[1]],
  uganda
) * 0 + 1

# Travel time to nearest city
travel_uganda <- travel_time(
  to = "city",
  size = 1,
  path = getwd()
) |>
  crop(uganda_mask) |>
  mask(uganda_mask)

# Rescale: 0 = difficult access, 1 = easy access
rescale_travel <- 1 - travel_uganda /
  global(travel_uganda, "max", na.rm = TRUE)[[1]]

# Load lake polygons
lakes <- vect("lakes_Clip1.shp")

# Match CRS
lakes <- project(lakes, crs(rescale_travel))

# Crop lakes to raster extent
lakes <- crop(lakes, ext(rescale_travel))

# Create lake mask: 1 = lake, 0 = land
lake_mask <- rasterize(
  lakes,
  rescale_travel,
  field = 1,
  background = 0
)

# Remove lake pixels
rescale_travel <- ifel(
  lake_mask == 1,
  NA,
  rescale_travel
)

# Save final raster
writeRaster(
  rescale_travel,
  "rescale_travel.tif",
  overwrite = TRUE
)

# Plot
plot(rescale_travel)
plot(lakes, add = TRUE, border = "red", lwd = 1.5)

# ============================================================
# EXTRACT TRAVEL ACCESSIBILITY
# ============================================================

# Project household points to travel raster CRS
hh_points_travel <- terra::project(
  hh_points,
  terra::crs(rescale_travel)
)

# Extract travel accessibility values
travel_values <- terra::extract(
  rescale_travel,
  hh_points_travel
)

# Add travel accessibility to household data
hh_env$travel <- travel_values[[2]]


# =======================
# 6. PREPARE CONTINENTAL OFFSET SURF
#========================

# Read continental prediction raster
continental <- terra::rast("uganda_preds_p.tif")

# Combine target vector predictions
# Band 3 = An. funestus
# Band 4 = An. gambiae
continental_offset <- continental[[3]] +
  continental[[4]]

names(continental_offset) <- "continental_offset"

# Project household points to continental raster CRS
hh_points_cont <- terra::project(
  hh_points,
  terra::crs(continental_offset)
)

# Extract continental predictions
continental_values <- terra::extract(
  continental_offset,
  hh_points_cont
) |>
  as_tibble()

names(continental_values)[2] <- "continental_offset"


# =================================
# 7. CREATE FINAL INTEGRATED DATASET
# =================================

ento_env <- hh_env |>
  left_join(
    continental_values,
    by = "ID"
  ) |>
  select(
    -ID,
    -longitude,
    -latitude
  )

# Check final dataset
glimpse(ento_env)
summary(ento_env)

# 7. CREATE FINAL INTEGRATED DATASET


ento_env <-
  hh_env |>
  
  left_join(
    continental_values,
    by = "ID"
  ) |>
  
  select(
    -ID,
    -longitude,
    -latitude
  )

# 8. PREPARE DATA FOR MODELLING

# Prepare data
model_data <- ento_env |>
  mutate(
    across(
      c(an_gambiae_total, people_under_net, n_nets,
        tmax, tmin, precip, continental_offset, travel),
      as.numeric
    ),
    house_type = factor(house_type),
    sprayed = factor(sprayed),
    month = factor(month(event_date))
  ) |>
  filter(
    is.finite(an_gambiae_total),
    is.finite(people_under_net),
    is.finite(n_nets),
    is.finite(tmax),
    is.finite(tmin),
    is.finite(precip),
    is.finite(travel),
    is.finite(continental_offset),
    continental_offset > 0
  ) |>
  mutate(
    log_offset = log(continental_offset)
  ) |>
  drop_na() |>
  droplevels()

# Check factor levels
cat("house_type:", nlevels(model_data$house_type), "levels\n")
cat("sprayed:", nlevels(model_data$sprayed), "levels\n")
cat("month:", nlevels(model_data$month), "levels\n")

# Remove single-level factors
if (nlevels(model_data$house_type) < 2)
  model_data$house_type <- NULL

if (nlevels(model_data$sprayed) < 2)
  model_data$sprayed <- NULL

if (nlevels(model_data$month) < 2)
  model_data$month <- NULL

# ============================================================
# MODEL FORMULAE
# ============================================================

formula_m1 <- as.formula(
  paste(
    "an_gambiae_total ~",
    paste(
      c(
        "people_under_net",
        "n_nets",
        if ("sprayed" %in% names(model_data)) "sprayed",
        if ("house_type" %in% names(model_data)) "house_type",
        if ("month" %in% names(model_data)) "month",
        "travel"
      ),
      collapse = " + "
    )
  )
)

formula_m2 <- update(
  formula_m1,
  . ~ . + tmax + tmin + precip
)

formula_m3 <- update(
  formula_m2,
  . ~ . + offset(log_offset)
)

# ============================================================
# FIT MODELS
# ============================================================

model_m1 <- glm.nb(
  formula_m1,
  data = model_data
)

model_m2 <- glm.nb(
  formula_m2,
  data = model_data,
  init.theta = model_m1$theta
)

model_m3 <- glm.nb(
  formula_m3,
  data = model_data,
  init.theta = model_m2$theta
)

# Store models
models <- list(
  M1 = model_m1,
  M2 = model_m2,
  M3 = model_m3
)

# Summaries
lapply(models, summary)

# Store models
models <- list(
  M1 = model_m1,
  M2 = model_m2,
  M3 = model_m3
)

# Summaries
lapply(models, summary)

# Model summaries
lapply(models, summary)


lapply(
  models,
  summary
)

# PREDICTIONS

pred_df <-
  
  tibble(
    
    obs = model_data$an_gambiae_total,
    
    M1 =
      predict(
        models$M1,
        type = "response"
      ),
    
    M2 =
      predict(
        models$M2,
        type = "response"
      ),
    
    M3 =
      predict(
        models$M3,
        type = "response"
      )
  )

# MODEL PERFORMANCE

metric_fun <- function(obs, pred){
  
  tibble(
    
    RMSE =
      sqrt(
        mean((obs - pred)^2)
      ),
    
    MAE =
      mean(
        abs(obs - pred)
      ),
    
    R2 =
      cor(
        obs,
        pred
      )^2
  )
}

model_metrics <-
  
  bind_rows(
    
    lapply(
      
      names(models),
      
      function(m){
        
        metric_fun(
          pred_df$obs,
          pred_df[[m]]
        ) |>
          
          mutate(
            model = m,
            .before = 1
          )
        
      }
      
    )
    
  )

print(model_metrics)

# 7. LONG-FORM PREDICTION DATA


pred_long <- pred_df |>
  
  pivot_longer(
    cols = -obs,
    names_to = "model",
    values_to = "predicted"
  ) |>
  
  mutate(
    residual = obs - predicted
  )

# 8. RESIDUAL PLOT

g1 <- ggplot(
  pred_long,
  aes(
    x = obs,
    y = residual
  )
) +
  
  geom_point(
    alpha = 0.3
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  
  facet_wrap(
    ~ model,
    scales = "free"
  ) +
  
  labs(
    x = "Observed Anopheles count",
    y = "Residual"
  ) +
  
  theme_minimal()


#print(g1)



# 9. OBSERVED VS PREDICTED PLOT

g2 <- ggplot(
  pred_long,
  aes(
    x = obs,
    y = predicted
  )
) +
  
  geom_point(
    alpha = 0.3
  ) +
  
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed"
  ) +
  
  facet_wrap(
    ~ model,
    scales = "free"
  ) +
  
  labs(
    x = "Observed Anopheles count",
    y = "Predicted Anopheles count"
  ) +
  
  theme_minimal()


#print(g2)



# 10. DHARMa DIAGNOSTICS


dharma_results <- lapply(
  models,
  
  function(model) {
    
    DHARMa::simulateResiduals(
      fittedModel = model,
      n = 1000,
      plot = FALSE
    )
  }
)

# PLOT DHARMa RESULTS

plot(
  dharma_results$M1
)

plot(
  dharma_results$M2
)

plot(
  dharma_results$M3
)



# DIAGNOSTIC TESTS


diagnostic_tests <- lapply(
  dharma_results,
  
  function(x) {
    
    list(
      uniformity = DHARMa::testUniformity(x),
      dispersion = DHARMa::testDispersion(x),
      zero_inflation = DHARMa::testZeroInflation(x)
    )
  }
)


diagnostic_tests


# 11. ENVIRONMENTAL PREDICTION RASTERS

env_pred <-bioclim_uganda[[c(
  5,
  6,
  12
)]]

names(env_pred) <- c(
  "tmax",
  "tmin",
  "precip"
)

# ============================================================
# 13. PREPARE PREDICTION RASTERS
# ============================================================

template <- env_pred[[1]]

# Continental offset
continental_offset_aligned <- terra::resample(
  continental_offset, template, method = "bilinear"
)

continental_offset_aligned <- terra::ifel(
  continental_offset_aligned <= 0,
  1e-6,
  continental_offset_aligned
)

log_offset <- log(continental_offset_aligned)
names(log_offset) <- "log_offset"

# Travel accessibility: 0 = difficult, 1 = easy
travel_pred <- terra::resample(
  rescale_travel, template, method = "bilinear"
)
names(travel_pred) <- "travel"


# ============================================================
# 14. HELPER FUNCTIONS
# ============================================================

make_numeric_raster <- function(template, value, name) {
  r <- template
  r[] <- value
  names(r) <- name
  r
}

make_factor_raster <- function(template, variable, data) {
  values <- droplevels(as.factor(data[[variable]]))
  categories <- levels(values)
  r <- template
  r[] <- which.max(table(values))
  r <- as.factor(r)
  levels(r) <- data.frame(
    ID = seq_along(categories),
    category = categories
  )
  names(r) <- variable
  r
}


# ============================================================
# 15. BUILD PREDICTION RASTER STACKS
# ============================================================

# Model 2: local + environmental predictors
model2_covariates <- c(
  make_numeric_raster(
    template,
    median(model_data$people_under_net, na.rm = TRUE),
    "people_under_net"
  ),
  make_numeric_raster(
    template,
    median(model_data$n_nets, na.rm = TRUE),
    "n_nets"
  ),
  make_numeric_raster(template, 7, "month"),
  make_factor_raster(template, "sprayed", model_data),
  make_factor_raster(template, "house_type", model_data),
  env_pred
)

# Align travel raster to prediction grid
travel_pred <- terra::resample(
  rescale_travel,
  template,
  method = "bilinear"
)

names(travel_pred) <- "travel"

# Model 3: Model 2 + travel + continental offset
model3_covariates <- c(
  model2_covariates,
  travel_pred,
  log_offset
)

# ============================================================
# 16. SPATIAL PREDICTION
# ============================================================
# Prediction
pred_model2 <- terra::predict(
  model2_covariates,
  models$M2,
  type = "response",
  na.rm = TRUE
)

pred_model3 <- terra::predict(
  model3_covariates,
  models$M3,
  type = "response",
  na.rm = TRUE
)

# Uganda boundary and lakes
uganda_boundary <- geodata::gadm(
  "UGA", level = 0, path = "."
)

lakes <- terra::vect("lakes_Clip1.shp")

# Match CRS to prediction raster
uganda_boundary <- terra::project(
  uganda_boundary,
  terra::crs(pred_model2)
)

lakes <- terra::project(
  lakes,
  terra::crs(pred_model2)
)

# Keep Uganda land and remove lakes
pred_model2 <- terra::mask(
  terra::mask(pred_model2, uganda_boundary),
  lakes,
  inverse = TRUE
)

pred_model3 <- terra::mask(
  terra::mask(pred_model3, uganda_boundary),
  lakes,
  inverse = TRUE
)

# Names and difference
names(pred_model2) <- "anopheles_local"
names(pred_model3) <- "anopheles_integrated"

prediction_difference <- pred_model3 - pred_model2
names(prediction_difference) <- "integrated_difference"
# ============================================================
# 17. PLOT SPATIAL PREDICTIONS
# ============================================================

zmax <- max(
  terra::global(pred_model2, "max", na.rm = TRUE)[1, 1],
  terra::global(pred_model3, "max", na.rm = TRUE)[1, 1]
)

lim <- max(abs(terra::minmax(prediction_difference)))

# Model 2
p1 <- ggplot() +
  tidyterra::geom_spatraster(data = pred_model2) +
  geom_sf(
    data = sf::st_as_sf(uganda_boundary),
    fill = NA, colour = "grey20", linewidth = 0.4
  ) +
  geom_sf(
    data = sf::st_as_sf(lakes),
    fill = "white", colour = NA
  ) +
  scale_fill_viridis_c(
    option = "C",
    limits = c(0, zmax),
    name = "Predicted\nmosquitoes"
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Model 2",
    subtitle = "Local + environmental predictors"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12),
    panel.grid = element_blank(),
    axis.title = element_blank()
  )


# Model 3
p2 <- ggplot() +
  tidyterra::geom_spatraster(data = pred_model3) +
  geom_sf(
    data = sf::st_as_sf(uganda_boundary),
    fill = NA, colour = "grey20", linewidth = 0.4
  ) +
  geom_sf(
    data = sf::st_as_sf(lakes),
    fill = "white", colour = NA
  ) +
  scale_fill_viridis_c(
    option = "C",
    limits = c(0, zmax),
    name = "Predicted\nmosquitoes"
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Model 3",
    subtitle = "Integrated + travel accessibility"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12),
    panel.grid = element_blank(),
    axis.title = element_blank()
  )


# Difference
p3 <- ggplot() +
  tidyterra::geom_spatraster(data = prediction_difference) +
  geom_sf(
    data = sf::st_as_sf(uganda_boundary),
    fill = NA, colour = "grey20", linewidth = 0.4
  ) +
  geom_sf(
    data = sf::st_as_sf(lakes),
    fill = "white", colour = NA
  ) +
  scale_fill_gradient2(
    low = "#3B4CC0",
    mid = "white",
    high = "#B40426",
    midpoint = 0,
    limits = c(-lim, lim),
    oob = scales::squish,
    name = "Difference"
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Integrated model effect",
    subtitle = "Model 3 − Model 2"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12),
    panel.grid = element_blank(),
    axis.title = element_blank()
  )


# ============================================================
# 18. COMBINE FIGURES
# ============================================================

final_plot <- (p1 | p2 | p3) +
  patchwork::plot_layout(
    ncol = 3,
    guides = "collect"
  ) +
  patchwork::plot_annotation(
    title = "Predicted An. gambiae abundance in Uganda",
    theme = theme(
      plot.title = element_text(
        face = "bold",
        size = 18,
        hjust = 0.5
      )
    )
  ) &
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.title = element_blank()
  )

final_plot


# ============================================================
# 19. SAVE FIGURE AND RASTERS
# ============================================================

ggsave(
  "Integrated_model_predictions.png",
  final_plot,
  width = 16,
  height = 6,
  dpi = 600,
  bg = "white"
)

dir.create("predictions", showWarnings = FALSE)

terra::writeRaster(
  pred_model2,
  "predictions/an_gambiae_model2.tif",
  overwrite = TRUE
)

terra::writeRaster(
  pred_model3,
  "predictions/an_gambiae_integrated_model.tif",
  overwrite = TRUE
)

terra::writeRaster(
  prediction_difference,
  "predictions/integrated_model_difference.tif",
  overwrite = TRUE
)




# 
# 
# 
# 
# 
# 
# 
# 
# sprayed_fun <- bc_uganda[[1]] %>%
#   (\(r) {
#     # initialize with NA
#     r[] <- NA
#     # assign yes/no factor levels
#     levels(r) <- data.frame(
#       id = 0:1,
#       sprayed = c("no", "yes")
#     )
#     # mark non-NA cells as sprayed = 1
#     not_na_idx <- which(!is.na(values(bc_uganda[[1]])))
#     r[not_na_idx] <- 1
#     r
#   })()
# 
# 
# #Here, the anonymous function 
# #\(r){...} lets you keep everything
# #inside the pipe, tidyverse-like.
# 
# 
# 
# has_net<-bc_uganda[[1]]
# 
# not_na_idx <- which(!is.na(values(has_net)))
# has_net[] <- NA
# levels(has_net) <- data.frame(
#   id = 0:1,
#   has_net = c("no", "yes")
# )
# has_net[not_na_idx] <- 1
# plot(has_net)
# 
# # save raster
# terra::writeRaster(
#   x = has_net,
#   filename = "has_net.tif"
# )
# 
# sprayed_before <- bc_uganda[[1]]
# nat_na_idx <- which(!is.na(values(sprayed_before)))
# sprayed_before[] <- NA
# levels(sprayed_before) <- data.frame(
#   id = 0:1,
#   sprayed_before = c("no", "yes")
# )
# sprayed_before[not_na_idx] <- 1
# plot(sprayed_before)
# 
# # save raster
# terra::writeRaster(
#   x = sprayed_before,
#   filename = "sprayed_before.tif"
# )
# 
# months_since_spray <- bc_uganda[[1]]
# not_na_idx <- which(!is.na(values(months_since_spray)))
# months_since_spray[] <- NA
# months_since_spray[not_na_idx] <- sample(0:12, length(not_na_idx), replace = TRUE)
# plot(months_since_spray)
# 
# # save raster
# terra::writeRaster(
#   x = months_since_spray,
#   filename = "months_since_spray.tif"
# )
# nets_owned <- bc_uganda[[1]]
# not_na_idx <- which(!is.na(values(nets_owned)))
# nets_owned[] <- NA
# nets_owned[not_na_idx] <- sample(0:5, length(not_na_idx), replace = TRUE)
# plot(nets_owned)
# # save raster
# terra::writeRaster(
#   x = nets_owned,
#   filename = "nets_owned.tif"
# )
# 
# people_sleeping <- bc_uganda[[1]]
# not_na_idx <- which(!is.na(values(people_sleeping)))
# people_sleeping[] <- NA
# people_sleeping[not_na_idx] <- sample(1:10, length(not_na_idx), replace = TRUE)
# plot(people_sleeping)
# 
# # save raster
# terra::writeRaster(
#   x = people_sleeping,
#   filename = "people_sleeping.tif"
# )
# 
# 
# net_type <- bc_uganda[[1]]
# not_na_idx <- which(!is.na(values(net_type)))
# net_type[] <- NA
# levels(net_type) <- data.frame(
#   id = 1:5,
#   net_type = c("Permanet","Permanet 3.0","PBO net","Olyset net","Olyset Plus")
# )
# net_type[not_na_idx] <- 1
# plot(net_type)
# 
# # save raster
# terra::writeRaster(
#   x = net_type,
#   filename = "net_type.tif"
# )
# 
# # Spatial prediction
# # Stack environmental covariates,bias layer and other predictors
# 
# env_bias_pred_layor <- rast(c("env_variables.tif","bias.tif",
#                               "has_net.tif", "sprayed.tif", 
#                               "sprayed_before.tif",
#                               "months_since_spray.tif", 
#                               "nets_owned.tif",
#                               "net_type.tif",
#                               "people_sleeping.tif"))
# 
# names(env_bias_pred_layor) <- c("tmax", "tmin", "precip","bias",
#                                 "has_net", "sprayed",
#                                 "sprayed_before",
#                                 "months_since_spray", 
#                                 "nets_owned",
#                                 "net_type",
#                                 "people_sleeping")
# 
# # predict our distribution based on our model and covariates
# pred_count_model3 <- sdmtools::predict_sdm(
#   model = count_model_3,
#   covariates =env_bias_pred_layor 
# )
# 
# plot(pred_count_model3 )
# 
# 
# # Use non random data
# # evaluation of the model
# 
# covs2 <- bc_uganda[[c(5,6,12)]]
# names(covs) <- c("tmax", "tmin", "precip")
# 
# terra::writeRaster(
#   x = covs2,
#   filename = "covariates2.tif",
#   overwrite=TRUE
# )
# 
# # create variation:
# 
# env_unscaled <-(c(covs2$wc2.1_30s_bio_5 * -0.03,
#                   covs2$wc2.1_30s_bio_6 * 0.8,
#                   covs2$wc2.1_30s_bio_12* 900))
# 
# names(env_unscaled) <- c("tmax", "tmin", "precip")
# 
# terra::writeRaster(
#   x = env_unscaled,
#   filename = "env_variables2.tif",
#   overwrite=TRUE
# )
# 
# 
# env_bias_pred_layor1 <- rast(c("env_variables2.tif","bias.tif",
#                                "has_net.tif","sprayed.tif", 
#                                "sprayed_before.tif",
#                                "months_since_spray.tif", 
#                                "nets_owned.tif",
#                                "net_type.tif",
#                                "people_sleeping.tif"))
# 
# names(env_bias_pred_layor1) <-c("tmax", "tmin", 
#                                 "precip","bias",
#                                 "has_net", "sprayed",
#                                 "sprayed_before",
#                                 "months_since_spray", 
#                                 "nets_owned",
#                                 "net_type",
#                                 "people_sleeping")
# 
# 
# pred_count_model3_1 <- sdmtools::predict_sdm(
#   model = count_model_3,
#   covariates =env_bias_pred_layor1 
# )
# 
# plot(pred_count_model3_1)
# 
# 
# simulationOutput <- simulateResiduals(fittedModel = count_model_3,
#                                       plot = FALSE)
# 
# # plot simulated residuals
# plot(simulationOutput)
# # err looks really friggin good
# 
# 
# 
# # Always compare vs fitted for any model (The difference between observed and predicted value)
# # These should be always always amost normally distributed
# 
# 
# 
# 
# # rescale the counts, from 0 to 1
# rel_count <- rescale_abundance(rel_abund_unscaled)
# 
# names(rel_abund) <- "relative_abundance"
# 
# plot(rel_abund)
# 
# terra::writeRaster(
#   x = rel_abund,
#   filename = "data/grids/rel_abund.tif"
# )
# 
# # sample abundance data at a random set of locations
# n_samples <- 100
# 
# # random locations all over the country - unweighted sampling
# sample_locations_random <- random_locations(kenya_mask,
#                                             n_samples,
#                                             weighted = FALSE)
# 
# plot(rel_abund)
# points(sample_locations_random)
# 
# catches_random <- sim_catches(sample_locations = sample_locations_random,
#                               relative_abundance = rel_abund,
#                               max_average_catch_size = 100)
# 
# plot(rel_abund)
# points(catches_random, pch = 21, bg = catches_random$presence)
# 
# # random locations, biased as per the bias layer - e.g. convenience samples
# sample_locations_bias_weighted <- random_locations(bias,
#                                                    n_samples)
# plot(bias)
# points(sample_locations_bias_weighted, pch = 16)
# catches_bias_weighted <- sim_catches(sample_locations = sample_locations_bias_weighted,
#                                      relative_abundance = rel_abund,
#                                      max_average_catch_size = 100)
# 
# plot(rel_abund)
# points(catches_bias_weighted, pch = 21, bg = catches_bias_weighted$presence)
# 
# 
# 
# # random locations, biased as per the relative abundance layer - e.g. targeted
# # to areas of high abundance (where malaria interventions happen?)
# sample_locations_abundance_weighted <- random_locations(rel_abund ^ (1/3),
#                                                         n_samples)
# 
# catches_abundance_weighted <- sim_catches(sample_locations = sample_locations_abundance_weighted,
#                                           relative_abundance = rel_abund,
#                                           max_average_catch_size = 100)
# 
# plot(rel_abund)
# points(catches_abundance_weighted,
#        pch = 21,
#        bg = catches_abundance_weighted$presence)
# 
# 
# # simulating biased occurrence data - there are two ways:
# 
# # 1. simulate biased sampling locations, and then sample the presence at those
# # locations, and only keep the ones in which they are present
# 
# # filter out biased ones to only the 1s, and store as coordinates of occurrence records
# sample_locations_bias_weighted <- random_locations(bias,
#                                                    n_samples)
# catches_bias_weighted <- sim_catches(sample_locations = sample_locations_bias_weighted,
#                                      relative_abundance = rel_abund,
#                                      max_average_catch_size = 100)
# occurrence_coords <- crds(catches_bias_weighted[catches_bias_weighted$presence == 1])
# 
# plot(kenya_mask)
# points(occurrence_coords, pch = 16)
# 
# # this is great, but when simulating, it's difficult to get a realistic number
# # of occurrence records for model fitting! You can try changing the number of
# # sampling locations (n_samples in the code above) until it looks good:
# 
# sample_locations_bias_weighted <- random_locations(bias,
#                                                    2000)  # <- change this number
# catches_bias_weighted <- sim_catches(sample_locations = sample_locations_bias_weighted,
#                                      relative_abundance = rel_abund,
#                                      max_average_catch_size = 100)
# occurrence_coords <- crds(catches_bias_weighted[catches_bias_weighted$presence == 1])
# 
# # number of records
# nrow(occurrence_coords)
# plot(kenya_mask)
# points(occurrence_coords, pch = 16)
# 
# 
# # 2. alternatively, you can simulate the distribution of occurrence points by
# # simulating locations, biased by the *product* of the bias and probability of
# # detection in each cell. The probability of detection is calculated using a
# # formula that assumes the occurrence comes from a catch (like the simulations
# # above), and that the number of mosquitoes caught is a Poisson sample, given
# # the average number. It is the probability of observing one *or more*
# # mosquitoes in the catch. 
# 
# prob_present <- probability_of_presence(rel_abund, max_average_catch_size = 100)
# names(prob_present) <- "prob_present"
# plot(prob_present)
# 
# reported_occurrence_rate <- bias * prob_present
# names(reported_occurrence_rate) <- "rep_occ_rate"
# 
# plot(reported_occurrence_rate)
# n_occurrences <- 100
# sample_locations_bias_weighted <- random_locations(reported_occurrence_rate,
#                                                    n_occurrences)
# 
# # plot the reported occurrence rates
# plot(reported_occurrence_rate)
# points(sample_locations_bias_weighted, pch = 16)
# 
# plot(
#   c(
#     rel_abund,
#     prob_present,
#     reported_occurrence_rate,
#     bias
#   )
# )
# points(occurrence_coords, cex = 0.2)
# 
# terra::writeRaster(
#   x = prob_present,
#   filename = "data/grids/prob_present.tif"
# )
# 
# terra::writeRaster(
#   x = reported_occurrence_rate,
#   filename = "data/grids/reported_occurrence_rate.tif"
# )
# 
# dir.create("data/tabular")
# # save occurrence data
# 
# # presence_only
# write.csv(
#   x = occurrence_coords,
#   file = "data/tabular/presence_only_data.csv",
#   row.names = FALSE
# )
# 
# # format presence absence data
# 
# # presence-absence data with sampling locations randomly selected
# pa_random_data <- as_tibble(catches_random) %>%
#   dplyr::select(count, presence) %>%
#   bind_cols(as_tibble(crds(catches_random)))
# 
# # # non-tidyverse way
# # pa_random_occurrence <- as.data.frame(catches_random)
# # pa_random_occurrence <- pa_random_occurrence[, c("count", "presence")]
# # pa_random_coords <- as.data.frame(crds(catches_random))
# # pa_random_data <- cbind(
# #   pa_random_occurrence,
# #   pa_random_coords
# # )
# 
# write.csv(
#   x = pa_random_data,
#   file = "data/tabular/presence_absence_random_sampling.csv",
#   row.names = FALSE
# )
# 
# # presence-absence data with sampling locations biased towards areas closer to
# # major cities
# pa_bias_data <- as_tibble(catches_bias_weighted) %>%
#   dplyr::select(count, presence) %>%
#   bind_cols(as_tibble(crds(catches_bias_weighted)))
# 
# write.csv(
#   x = pa_bias_data,
#   file = "data/tabular/presence_absence_bias_sampling.csv",
#   row.names = FALSE
# )
# 
# # presence-absence data with sampling locations biased towards areas with higher
# # abundance
# pa_bias_abund_data <- as_tibble(catches_abundance_weighted) %>%
#   dplyr::select(count, presence) %>%
#   bind_cols(as_tibble(crds(catches_abundance_weighted)))
# 
# write.csv(
#   x = pa_bias_abund_data,
#   file = "data/tabular/presence_absence_bias_abund_sampling.csv",
#   row.names = FALSE
# )
# 
# # to do:
# 
# # output 4x rasters:
# #  rel_abund
# #  prob_present,
# #  bias
# #  reported_occurrence_rate
# 
# # output 4x datasets:
# #  occurrence_coords (presence-only coordinates biased towards major cities)
# #  pa_random_data (presence-absence, locations randomly sampled)
# #  pa_bias_data (presence-absence, locations biased towards major cities)
# #  pa_bias_abund_data (presence-absence, locations biased towards higher
# #      abundance areas)
# 
# 
# 
# 
# 
# 
# 
# 
# # Ug bb
# lon_min <- 29.5
# lon_max <- 35.0
# lat_min <- -1.5
# lat_max <- 4.5
# 
# #generate 500 random coordinates
# coords <- data.frame(
#   lon = runif(500, lon_min, lon_max),
#   lat = runif(500, lat_min, lat_max)
# )
# 
# # convert to SpatVector
# pts <- vect(coords, geom = c("lon", "lat"), crs = crs(env_bias_layor))
# 
# #extract covariate values
# cov_vals <- terra::extract(env_bias_layor, pts) %>%
#   select(-ID)
# 
# # predict using model3 since it
# #has env and bias that can have spatial data 
# pred3 <- predict(count_model_3.1, newdata = cov_vals, type = "response")
# 
# # combine lon/lat with predictions
# pred3_df <- coords %>%
#   mutate(prediction = pred3)
# 
# # plot
# prediction_plot <- ggplot(pred3_df, aes(x = lon, y = lat, color = prediction)) +
#   geom_point(size = 2) +
#   scale_color_viridis_c() +
#   coord_fixed() +
#   labs(x = "Longitude", y = "Latitude", color = "Predicted count") +
#   theme_minimal()
# 
# 
# 
# library(terra)
# library(dplyr)
# library(purrr)
# 
# # --- inputs you must set ---------------------------------------------------
# # fitted model
# model <- count_model_2
# 
# # folder or specific rasters already loaded
# # example: tseas, tmax, trange, bias are SpatRaster objects available in env
# # if you have files instead:
# # files <- c("data/grids/tseas.tif","data/grids/tmax.tif","data/grids/trange.tif","data/grids/bias.tif")
# # env_stack <- rast(files)
# 
# # use rasters already in memory (from your script)
# env_stack <- covs2
# # --------------------------------------------------------------------------
# 
# # ensure stack has names
# # set these to the variable names your model expects for those layers
# # e.g. if model uses "tseas","tmax","trange","bias" keep those names
# names(env_stack) <- names(env_stack)
# 
# # 1) extract variable names from model (predictors only)
# all_vars <- all.vars(formula(model))
# resp <- all_vars[1]                     # response (left of ~)
# pred_vars <- all_vars[-1]               # may include function names; safer to parse terms
# pred_vars <- attr(terms(model), "term.labels")  # robust predictor labels
# 
# # 2) determine which predictors are already available as raster layers
# raster_vars <- names(env_stack)
# present <- pred_vars %in% raster_vars
# 
# # 3) build a stack that contains all predictors required by the model.
# #    for predictors not available as rasters, create constant rasters using
# #    summary values from the training data (mean for numeric, reference level for factor).
# train_df <- model.frame(model)  # data used to fit model
# 
# # template raster for extent/resolution/proj
# template <- env_stack[[1]]
# 
# # helper to make constant numeric raster
# const_rast_num <- function(val, template) {
#   r <- template
#   r[] <- val
#   r
# }
# 
# # helper to make constant factor raster with same levels as training data
# const_rast_factor <- function(varname, train_df, template) {
#   levs <- levels(factor(train_df[[varname]]))
#   r <- template
#   # store integer codes (1..n)
#   # choose first level as default (you can change)
#   r[] <- as.integer(factor(levs[1], levels = levs))
#   r <- as.factor(r)
#   levels(r) <- data.frame(id = seq_along(levs), label = levs)
#   r
# }
# 
# # assemble final list of rasters in model order
# final_layers <- vector("list", length(pred_vars))
# names(final_layers) <- pred_vars
# 
# for (v in pred_vars) {
#   if (v %in% raster_vars) {
#     final_layers[[v]] <- env_stack[[v]]
#   } else {
#     # predictor missing as raster: infer type from training data
#     if (is.numeric(train_df[[v]])) {
#       val <- mean(train_df[[v]], na.rm = TRUE)
#       final_layers[[v]] <- const_rast_num(val, template)
#       names(final_layers[[v]]) <- v
#     } else {
#       # treat as factor/character
#       final_layers[[v]] <- const_rast_factor(v, train_df, template)
#       names(final_layers[[v]]) <- v
#     }
#   }
# }
# 
# # create SpatRaster stack in the same order as model predictors
# env_stack_new <- rast(final_layers)
# names(env_stack_new) <- pred_vars
# 
# # 4) ensure factor levels in rasters match model$xlevels
# # terra stores factor levels in raster; glm prediction uses model's xlevels.
# # For safety, set levels in raster layers for factor predictors to equal model levels.
# if (!is.null(model$xlevels)) {
#   for (fn in names(model$xlevels)) {
#     if (fn %in% names(env_stack_new)) {
#       # convert raster to factor if not already, and set levels to model levels
#       r <- env_stack_new[[fn]]
#       if (!is.factor(r)) r <- as.factor(r)
#       levs <- model$xlevels[[fn]]
#       levels(r) <- data.frame(id = seq_along(levs), label = levs)
#       env_stack_new[[fn]] <- r
#     }
#   }
# }
# 
# # 5) Predict with terra::predict and save to disk
# # note: terra::predict will call predict() on the model for each cell.
# #       if your model contains smooth spatial terms s(x,y) or complex random effects,
# #       this may not be supported directly. See note below.
# out_file <- "predictions/count_model_2_pred.tif"
# pred_raster <- terra::predict(env_stack_new,
#                               model, 
#                               type = "response", 
#                               file = "out_file", 
#                               overwrite = TRUE)
# 
# # quick plot
# plot(pred_raster, main = "Predicted counts (count_model_2)")
# 
# 
# # Predict using count_model_2
# pred_count_model2 <- sdmtools::predict_sdm(
#   model      = count_model_2,
#   covariates = env_bias_pred_layor1   # <- your raster stack of covariates
# )
# 
# # Plot predictions
# plot(pred_count_model2, 
#      main = "Predicted counts from count_model_2")


