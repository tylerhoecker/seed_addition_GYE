library(tidyverse)
library(sf)
library(viridis)


perim_path <- '/Users/tylerhoecker/GitHub/seed_addition_GYE/data/fire_perimeters/'

fires <- c('Berry','Buffalo','Maple')

# Import fire perimeters and combine features
berry <- st_read(paste0(perim_path,fires[1],'Fire')) %>% 
  rename(acres = IR_Acres)
buffalo <- st_read(paste0(perim_path,fires[2],'Fire')) %>% 
  select(Id, acres, geometry)
maple <- st_read(paste0(perim_path,fires[3],'Fire')) %>% 
  select(Id, acres, geometry)

perimeters <- rbind(berry,buffalo,maple)

rm(berry,buffalo,maple)

# Import aspect 
#library(raster)
#library(rgdal)

gen_path <- '/Users/tylerhoecker/GitHub/seed_addition_GYE/data/general_spatial/'

DEM <- raster::raster('data/general_spatial/GYE_DEM_NAD83_UTM12N_30m_cuco.img')

DEM_crop <- raster::crop(DEM, as.vector(st_bbox(perimeters))) 

rm(DEM)
  
DEM_spdf <- as(DEM_crop, "SpatialPixelsDataFrame")

rm(DEM_crop)

DEM_df <- as.data.frame(DEM_spdf)

rm(DEM_spdf)

colnames(DEM_df) <- c("value", "x", "y")

ggplot() + 
geom_tile(data = DEM_df, aes(x=x, y=y, fill=value)) +
geom_sf(data = perimeters) 

