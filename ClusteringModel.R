library(tidycensus)
library(dplyr)
library(stringr)
library(leaflet)

setwd("~/GitHub/DS-Now-Final-Project")

#Load the data
load("census_data.RData")

#Get rid of margins of error for now
minus_margins_tr <- stJoesTract %>%
  select(-ends_with('M', ignore.case = F))

#Get rid of margins of error for now
minus_margins_bg <- stJoesBlockGroup %>%
  select(-ends_with('M', ignore.case = F))

#Find the number of missing values for each dataset
tract_nas <- sapply(minus_margins_tr, function(x){sum(is.na(x))})
bg_nas <- sapply(minus_margins_bg, function(x){sum(is.na(x))})

bg_nas[bg_nas > 0]

#Filter columns with an NA's
filtered_bg <- minus_margins_bg[, bg_nas == 0]

#Filter columns with an NA's
filtered_tr <- minus_margins_tr[, tract_nas == 0]

#Scale values for clustering
scaled_bg <- filtered_bg %>%
  select_if(is.numeric) %>%
  scale() %>%
  as_tibble()

scaled_tract <- filtered_tr %>%
  select_if(is.numeric) %>%
  scale() %>%
  as_tibble()

num_centers <- seq(from = 2, to = 15, by = 1)
tot_withinss_bg <- rep(0, length(num_centers))
tot_withinss_tr <- rep(0, length(num_centers))

for (i in 1:length(num_centers)) {
  kmeans_model_bg <- kmeans(scaled_bg, num_centers[i])
  
  tot_withinss_bg[i] <- kmeans_model_bg$tot.withinss
  
  kmeans_model_tr <- kmeans(scaled_tract, num_centers[i])
  
  tot_withinss_tr[i] <- kmeans_model_tr$tot.withinss
}

plot(num_centers, tot_withinss_tr)
plot(num_centers, tot_withinss_bg)

kmeans_five_bg <- kmeans(scaled_bg, 5)
center_info_bg <- data.frame(kmeans_five_bg$centers)
kmeans_five_bg$size

kmeans_five_tr <- kmeans(scaled_tract, 5)
center_info_tr <- data.frame(kmeans_five_tr$centers)
kmeans_five_tr$size

scaled_scale_tr <- attr(scaled_tract$total_populationE, 'scaled:scale')
scaled_center_tr <- attr(scaled_tract$total_populationE, 'scaled:center')
unscaled_centers_tr <- center_info_tr

for (i in 1:ncol(center_info_tr)) {
  name_of_column <- colnames(center_info_tr)[i]
  
  unscaled_centers_tr[, name_of_column] <- center_info_tr[, name_of_column] * scaled_scale[name_of_column] + scaled_center[name_of_column]
}

scaled_scale_bg <- attr(scaled_bg$total_populationE, 'scaled:scale')
scaled_center_bg <- attr(scaled_bg$total_populationE, 'scaled:center')
unscaled_centers_bg <- center_info_bg

for (i in 1:ncol(center_info_bg)) {
  name_of_column <- colnames(center_info_bg)[i]
  
  unscaled_centers_bg[, name_of_column] <- center_info_bg[, name_of_column] * scaled_scale[name_of_column] + scaled_center[name_of_column]
}

load("census_shapefiles.RData")

block_group_map_data_bg <- blockGroupShapes %>%
  left_join(filtered_bg, by = "NAME") %>%
  mutate(Profile = kmeans_five_bg$cluster)

clusterPal_bg <- colorFactor(c("red", "orange", "yellow", "green", "blue"), block_group_map_data_bg$Profile)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = block_group_map_data_bg,
              color = ~clusterPal_bg(Profile),
              opacity = 1) %>%
  addLegend(pal = clusterPal_bg,
            values = block_group_map_data$Profile)


block_group_map_data_tr <- tractShapes %>%
  left_join(filtered_tr, by = "NAME") %>%
  mutate(Profile = kmeans_five_tr$cluster)

clusterPal_tr <- colorFactor(c("red", "orange", "yellow", "green", "blue"), block_group_map_data_tr$Profile)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = block_group_map_data_tr,
              color = ~clusterPal_tr(Profile),
              opacity = 1) %>%
  addLegend(pal = clusterPal_tr,
            values = block_group_map_data$Profile)



######Decent amount of missing values for block groups. Will try to fill in with median from census tract######
#First, find which columns are numeric
numeric_columns <- unlist(lapply(minus_margins_tr, is.numeric))  

#Get the columns that will be filled in
numeric_col_names <- colnames(minus_margins_tr)[numeric_columns]

#Progromatically go through and fill-in missing block group data with census tract data
for (i in 1:length(numeric_col_names)) {
  filledInData <- DataCombine::FillIn(minus_margins_bg,
                                      minus_margins_tr[, c("CensusTract", numeric_col_names[i])],
                                      numeric_col_names[i],
                                      numeric_col_names[i],
                                      KeyVar = c("CensusTract"),
                                      allow.cartesian = FALSE,
                                      KeepD2Vars = FALSE)
}
