setwd("~/GitHub/DS-Now-Final-Project")

library(tidycensus)
library(tidyverse)
library(leaflet)

load("clustering_work.RData")
load("census_shapefiles.RData")

block_group_map_data_bg <- blockGroupShapes %>%
  left_join(filtered_bg, by = "NAME") %>%
  mutate(Profile = kmeans_five_bg$cluster)

clusterPal_bg <- colorFactor("Blues", block_group_map_data_bg$Profile)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = block_group_map_data_bg,
              color = ~clusterPal_bg(Profile),
              opacity = 1) %>%
  addLegend(pal = clusterPal_bg,
            values = block_group_map_data_bg$Profile)


block_group_map_data_tr <- tractShapes %>%
  left_join(filtered_tr, by = "NAME") %>%
  mutate(Profile = kmeans_five_tr$cluster)

clusterPal_tr <- colorFactor("Blues", block_group_map_data_tr$Profile)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = block_group_map_data_tr,
              color = ~clusterPal_tr(Profile),
              opacity = 1) %>%
  addLegend(pal = clusterPal_tr,
            values = block_group_map_data_tr$Profile)

ggplot(
  data = minus_margins_bg,
  aes(x = total_populationE, y = total_hous_unitsE, color = as.factor(cluster))
) + geom_point() +
  labs(color = "Profile",
       y = "Population",
       x = "Housing Units",
       title = "Block Group Population vs Housing Units")
