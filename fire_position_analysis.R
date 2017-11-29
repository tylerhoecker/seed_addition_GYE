library(tidyverse)
library(sf)
library(viridis)



GIS_path <- '/Users/tylerhoecker/Box Sync/PhD/GIS/GYE/'

fire_aspect_rast <- raster::brick(paste0(GIS_path,'aspect_fire_rast.tif'))

fire_spdf <- as(fire_aspect_rast, "SpatialPixelsDataFrame")




fire_aspect_df <- as(fire_aspect_rast, 'data.frame')

#
aspect_rast <- raster::raster(paste0(GIS_path,'aspect_fire_rast.tif'), band = 2)

aspect_spdf <- as(aspect_rast, "SpatialPixelsDataFrame")

fire_df <- as(aspect_fire_spdf, 'data.frame')

colnames(DEM_df) <- c("value", "x", "y")

ggplot() + 
geom_tile(data = DEM_df, aes(x=x, y=y, fill=value)) +
geom_sf(data = perimeters) 

