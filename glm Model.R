
#Variables
# y=gambiae_count
# x=n_nets_per_hh, people_sleeping, has_net, sprayed,

#Build a glm for the counts

library(terra)
library(tidyverse)
#library(predicts) # remotes::install_github("rspatial/predicts")
#library(glmnet)
library(maxnet)
library(tidyterra)
library(terra)
library(geodata)
library(traveltime)
library(sdmtools)#


# read in my data
simulated_data <- read_csv("simulated_data.csv") |> 
  mutate(
    net_type = as.factor(net_type),
    sprayed = as.factor(sprayed),
    has_net = as.factor(has_net),
    sprayed_before = as.factor(sprayed_before)
  )

# read in raster data

uganda_mask <- terra::rast("uganda_mask.tif")
bc_uganda <- terra::rast("bc_uganda.tif")
rescale_travel <- terra::rast("rescale_travel.tif")


# subset bc_keyna to our covariate set (and save this again for ease of use)
covs <- bc_uganda[[c(4,5,7)]]
names(covs) <- c("tseas", "tmax", "trange")

terra::writeRaster(
  x = covs,
  filename = "covariates.tif"
)

# download climactic data for uganda
bioclim_uganda1 <- worldclim_country(
  country = "UGA",
  var = "bio",
  res = 0.5,
  path = getwd()
)

env_var<-bioclim_uganda1[[c(5, 6, 12)]]

hh_coord<-read_csv("household_coordinates.csv") 

# convert household coords to SpatVector
hh_points <- vect(hh_coord, geom = c("lon", "lat"), crs = crs(env_var))

# extract raster values at points
hh_env <- terra::extract(env_var, hh_points) |> 
  as_tibble() 


#Create ID column to join on
hh_coord1<-hh_coord |>
 mutate(ID=row_number())

#Join env data to hh coord data

hh_env <- hh_coord1 |>
  left_join(hh_env, by = c("ID" = "ID")) |> 
  select(-ID)


# Join env data to sim data

simulated_data1<-simulated_data |> 
  left_join(hh_env, by = c("id" = "id","lon"="lon","lat"="lat")) |> 
  dplyr::rename(tmax =  "wc2.1_30s_bio_5",
                tmin = "wc2.1_30s_bio_6"   ,
                precip = "wc2.1_30s_bio_12")



# Read in the bias layer

# load bias raster (if already saved)
bias <- rast("bias.tif")

# convert household coordinates to SpatVector
hh_points <- vect(hh_coord, geom = c("lon", "lat"), crs = crs(bias))


# extract bias values
hh_bias <- terra::extract(bias, hh_points) |> 
  as_tibble()

#Create ID column to join on
hh_coord2<-hh_coord |>
  mutate(ID=row_number())

#Join bias data to hh coord data

hh_bias <- hh_coord2 |>
  left_join(hh_bias, by = c("ID" = "ID")) |> 
  select(-ID)

# Join env data to sim data

simulated_data2<-simulated_data1 |> 
  left_join(hh_bias, by = c("id" = "id","lon"="lon","lat"="lat")) 

# count model
count_model_1<- glm(
  gambiae_count ~ people_sleeping +
    nets_owned + net_type + sprayed + has_net + 
    sprayed_before ,
  data =simulated_data2 ,
  family = stats::poisson())


# count model that includes env data

count_model_2<- glm(
  gambiae_count ~ people_sleeping +
    nets_owned + net_type + sprayed + has_net + 
    sprayed_before + tmax + tmin + precip,
  data =simulated_data2 ,
  family = stats::poisson())

summary(count_model_2)

count_model_3<- glm(
  gambiae_count ~ people_sleeping +
    nets_owned + net_type + sprayed + has_net + 
    sprayed_before + tmax + tmin + precip+ bias,
  data =simulated_data2 ,
  family = stats::poisson())


summary(count_model_3)


# Spatial prediction










# predict on the data used to fit the model

pred1 <- predict(count_model_1, 
                 newdata = simulated_data2, type = "response")
pred2 <- predict(count_model_2, 
                 newdata = simulated_data2, type = "response")
pred3 <- predict(count_model_3, 
                 newdata = simulated_data2, type = "response")

# Data frame of observed and predicted values
pred_df <- data.frame(
  obs = simulated_data2$gambiae_count,
  m1 = pred1,
  m2 = pred2,
  m3 = pred3
) |> 
  as_tibble()

library(yardstick)   # for RMSE / R²
#Quantifying how well the model fits the data
# confusion matrix
# class probability summaries
# regression metrics

#Residuals 

resid_df <- pred_df %>%
  mutate(
    resid_m1 = obs - m1,
    resid_m2 = obs - m2,
    resid_m3 = obs - m3
  ) %>%
  pivot_longer(cols = starts_with("resid"), 
               names_to = "model", 
               values_to = "residual")

g1<-ggplot(resid_df, aes(x = obs, y = residual, color = model)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ model) +
  labs(x = "Observed counts", y = "Residuals") +
  theme_minimal()

g1

#RMSE and R²
metrics_df <- pred_df %>%
  pivot_longer(cols = starts_with("m"), 
               names_to = "model", 
               values_to = "pred") %>%
  group_by(model) %>%
  yardstick::metrics(truth = obs, estimate = pred) %>%
  filter(.metric %in% c("rmse","rsq"))

print(metrics_df)

# Observed vs Predicted scatter

pred_long <- pred_df %>%
  pivot_longer(cols = starts_with("m"),
               names_to = "model", 
               values_to = "pred")

g2<-ggplot(pred_long, aes(x = obs, y = pred, color = model)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ model) +
  labs(x = "Observed counts", y = "Predicted counts") +
  theme_minimal()

print(g2)

