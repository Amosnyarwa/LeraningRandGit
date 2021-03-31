library(sf)
library(tidyverse)
library(lwgeom)
 # read csv
settlement_data <- read_csv(file = "inputs/Total_hhs_03_2019.csv")

# read shapefile
settlement_shape <- st_read("inputs/Settlements_2019.shp", crs=4326)
sample_per_settlement <- rep(100, nrow(settlement_shape))

df_sample <- settlement_shape %>% 
  st_sample(sample_per_settlement)




ggplot() + 
  geom_sf(data = settlement_shape) + 
  geom_sf(data = df_sample)

?read_csv()
?join()
