
library(traveltime)
library(tidyverse)
library(geodata)
library(terra)
library(tidyterra)
library(sdmtools)# To calculate points outside the terra object

# get administrative area for a country
uganda_shp <- gadm(
  country = "UGA",
  level = 0,
  path = "/Users/alexm/Desktop/PH/Training/Resistance project/Travel time"
)

# have a look at it
plot(uganda_shp)

# download climactic data for this country
bioclim_uganda <- worldclim_country(
  country = "UGA",
  var = "bio",
  res = 0.5,
  path = "/Users/alexm/Desktop/PH/Training/Resistance project/Travel time"
)

# subset to just
# BIO5 = Max Temperature of Warmest Month
# BIO6 = Min Temperature of Coldest Month
# BIO12 = Annual Precipitation
covs_uganda <- bioclim_uganda[[c(5, 6, 12)]] |>
  # and mask to country shapefile
  mask(uganda_shp)

names(covs_uganda) <- c("tmax_warm", "tmin_cool", "precip")

# have a look
plot(covs_uganda)

# make up some points that fall within the boundary of your
# shapefile

uganda_points <- read.csv("res_data_ug.csv", sep =";") |>
rename(x = longitude, y = latitude) |>
  dplyr::mutate(x=str_replace(x, ",","."),
                y=str_replace(y, ",",".")) |> 
  dplyr::select(x, y) |>
  dplyr::mutate(x= parse_number(x),
                y=parse_number(y))
  
# check that the points fall inside and edit above until they do
plot(uganda_shp)
points(uganda_points)

# see points against each of the layers
par(mfrow = c(2,2))
plot(covs_uganda[[1]])
points(uganda_points)

plot(covs_uganda[[2]])
points(uganda_points)

plot(covs_uganda[[3]])
points(uganda_points)

par(mfrow = c(1,1))

#####
# turn covariates into raster package format for mess
library(dismo)
library(raster)

# extract covariate values at our points
coord_covs <- extract(
  covs_uganda,
  uganda_points
)

# convert layers to raster package for mess
covraster <- raster::brick(covs_uganda)

# calculate multivariate environmental similarity surface (MESS)
# for our points against whole raster
mess_uganda <- mess(
  x = covraster,
  v = coord_covs |>
    dplyr::select(-ID) |>
    as.data.frame()
) |>

# we then convert back to terra for nicer plotting
# and mask based on our earlier raster to remove Inf
# calculations from NA areas
rast() |>
  mask(covs_uganda[[1]])

# plot the result
plot(mess_uganda)
points(uganda_points)
# this result is in a unitless format but the closer to zero
# the value is, the more similar

par(mfrow = c(2,2))
plot(covs_uganda[[1]])
points(uganda_points)

plot(covs_uganda[[2]])
points(uganda_points)

plot(covs_uganda[[3]])
points(uganda_points)

plot(mess_uganda)
points(uganda_points)

par(mfrow = c(1,1))

# make plot limits for diverging palette around zero
plot_limits <- max(
  abs(values(mess_uganda)),
  na.rm = TRUE
) * c(-1, 1)


# plot with palette that diverges around zero
plot_mess_local <- ggplot() +
  geom_spatraster(
    data = mess_uganda
  ) +
  scale_fill_distiller(
    type = "div",
    palette = "RdBu",
    direction = 1,
    limit = plot_limits
  ) +
  theme_void() +
  labs(fill = "Multivariate\nEnvironmental\nSimilarity")

plot_mess_local


# make mask of this, such that anything < 0 is NA,
# i.e. dissimilar, and >= 0 is 1, i.e., similar.
mess_mask <- mess_uganda

mvals <- values(mess_uganda)

mess_mask[which(mvals < 0)] <- NA
mess_mask[which(mvals >= 0)] <- 1

mess_mask <- mask(mess_mask, uganda_shp)

points1<- uganda_points |>
  as_tibble() |>
  dplyr::rename(longitude=x,latitude=y)
# check which points fall inside the mask


points_inside<-sdmtools::inside_mask(points1,mess_mask)

# Difference between points1 and points_inside are the points that fall outside the mask  

y=setdiff(points1,points_inside)
 


# look at area of country we have represented
plot(mess_mask)
plot(uganda_shp, add = TRUE)
points(uganda_points)

# check the covariates we have represented
masked_covs <- mask(covs_uganda, mess_mask)
plot(masked_covs)


