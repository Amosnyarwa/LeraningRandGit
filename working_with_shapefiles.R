library(sf)
library(tidyverse)
help(sf)

library(lwgeom)

# read shapefile
shape <- st_read("inputs/Districts_2020.shp", crs=4326 )
sample_per_district <- rep(100, nrow(shape))

# creat samples
df_sample <- shape %>% 
  st_sample(sample_per_district)

ggplot() + 
  geom_sf(data = shape) + 
  geom_sf(data = df_sample)
