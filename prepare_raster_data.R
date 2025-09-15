library(tidyverse)
library(terra)
library(geodata)



# get extent map for our basic analyses - using outline of Kenya

uganda_vector <- gadm(
  country = "UGA",
  level = 0,
  path = getwd()
)

  # get extent map for our basic analyses - using outline of Kenya
  
  uganda_vector <- gadm(
    country = "UGA",
    level = 0,
    path = getwd()
  ) # slow to run though not large
  
  uganda_vector
  
  plot(uganda_vector)
  
  # let's download some data
  
  # travel time from city to cell
  # you can explore the sizes of city using ?travel time, 
  ##and change size if you wish
  
  travel <- travel_time(to = "city", 
                        size = 1, 
                        path = getwd())

  plot(uganda_vector, add = TRUE, border = "green")

  
  # 421 MB - may be slow to download
  
  # it's a good idea to have a look at your objects and even plot them
  
  
  # bioclimactic variables from worldclim
  # https://worldclim.org/data/bioclim.html
  bioclim_uganda <- worldclim_country(
    country = "UGA",
    var = "bio",
    res = 0.5,
    path = getwd()
  )
  
  # have a look at this object first
  # Do you think you don't want to plot all layers at once?
  
  # process these data to match our extent
  
  # generate mask of our area of interest so we can use 
  #it to process other data into this shape
  
  uganda_mask <- bioclim_uganda[[1]] %>%
    mask(uganda_vector) * 0 + 1
  
  plot(uganda_mask)
  
  
  # process bioclim into shape
  bc_uganda <- mask(
    x = bioclim_uganda,
    mask = uganda_mask
  )
  
  # process travel data
  # this is a very large raster, 
  #we speed up the operation by cropping then masking.
  # try looking at the result of only 
  #cropping to understand why both steps are needed
  
  # To check
  travel_uganda <- crop(
    x = travel,
    y = uganda_mask
  ) %>%
    mask(
      mask = uganda_mask
    )
  
  
  plot(travel_uganda)
  
  rescale_travel <- travel_uganda
  
  # Rescale 0–1 bias layer (0=hard, 1=easy)
  rescale_travel <- travel_uganda / global(travel_uganda, "max", na.rm = TRUE)[[1]]
  rescale_travel <- 1 - rescale_travel
  
  plot(rescale_travel)
  
  # now let's create a bias layer out of our travel data
  # it should scale from 0-1 where 0 is hard to get to and 1 is easy
  
  plot(travel_uganda)
  
  rescale_travel <- travel_uganda
  
  rescale_travel[] <- rescale_travel[]/ max(rescale_travel[], na.rm = TRUE)
  
  rescale_travel[] <- 1 - rescale_travel[]
  
  plot(rescale_travel)
  
  
  
  # now let's write our key rasters to disk
  # NB the above download functions will only need to download once;
  # once those objects are saved in that location, they will be re-read from disk
  
  terra::writeRaster(
    x = uganda_mask,
    filename = "uganda_mask.tif"
  )
  
  terra::writeRaster(
    x = bc_uganda,
    filename = "bc_uganda.tif"
  )

  terra::writeRaster(
    x = rescale_travel,
    filename = "rescale_travel.tif",
  )
  
  
  
  # bias layer
  bias <- rescale_travel ^ 2
  names(bias) <- "bias"
  
  terra::writeRaster(
    x = bias,
    filename = "bias.tif"
  )
  # BIO1 = Annual Mean Temperature
  # 
  # BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
  # 
  # BIO3 = Isothermality (BIO2/BIO7) (×100)
  # 
  # BIO4 = Temperature Seasonality (standard deviation ×100)
  # 
  # BIO5 = Max Temperature of Warmest Month
  # 
  # BIO6 = Min Temperature of Coldest Month
  # 
  # BIO7 = Temperature Annual Range (BIO5-BIO6)
  # 
  # BIO8 = Mean Temperature of Wettest Quarter
  # 
  # BIO9 = Mean Temperature of Driest Quarter
  # 
  # BIO10 = Mean Temperature of Warmest Quarter
  # 
  # BIO11 = Mean Temperature of Coldest Quarter
  # 
  # BIO12 = Annual Precipitation
  # 
  # BIO13 = Precipitation of Wettest Month
  # 
  # BIO14 = Precipitation of Driest Month
  # 
  # BIO15 = Precipitation Seasonality (Coefficient of Variation)
  # 
  # BIO16 = Precipitation of Wettest Quarter
  # 
  # BIO17 = Precipitation of Driest Quarter
  # 
  # BIO18 = Precipitation of Warmest Quarter
  # 
  # BIO19 = Precipitation of Coldest Quarter
  
  
  #plot(bc_kenya[[1:10]])
  #plot(bc_kenya[[11:19]])
  # 1, 5, 6, 8, 9, 10, 11 + 1; 12, 13, 14, 16, 17, 18, 19 -1
  # 2, 7, + 1
  # 3, 4, -1
 # slow to run though not large
