library(tidycensus)
library(dplyr)
library(stringr)
library(leaflet)
library(mice)
library(MissMech)

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
tr_nas <- sapply(minus_margins_tr, function(x){sum(is.na(x))})
bg_nas <- sapply(minus_margins_bg, function(x){sum(is.na(x))})

#Missing data will be imputed for variables with less that 10% missing
missing_data_cutoff_bg <- round(nrow(stJoesBlockGroup) * .1, 0)
missing_data_cutoff_tr <- round(nrow(stJoesTract) * .1, 0)

#Find which variables will be kept after imputation
bg_vars <- names(bg_nas[bg_nas < missing_data_cutoff_bg])
tr_vars <- names(tr_nas[tr_nas < missing_data_cutoff_tr])

#Also eliminate vars with only one value
bg_unique <- sapply(minus_margins_bg, function(x){length(unique(x))})
tr_unique <- sapply(minus_margins_tr, function(x){length(unique(x))})

#Filter columns with less than required NA's or 1 unique value
filtered_bg <- minus_margins_bg[, bg_nas < missing_data_cutoff_bg & bg_unique > 1]

#For the tracts, limit to what is in the block groups
filtered_tr <- minus_margins_tr[, colnames(minus_margins_tr) %in% colnames(filtered_bg)]

#Impute missing data
imputed_data_bg <- mice(select_if(filtered_bg, is.numeric),
                        m = 10,
                        maxit = 20,
                        pred = quickpred(select_if(filtered_bg, is.numeric),
                                         minpuc = .2,
                                         mincor = .01), 
                        print = FALSE,
                        seed = 300)

plot(imputed_data_bg, layout = c(2, 1))

imputed_data_tr <- mice(select_if(filtered_tr, is.numeric),
                        m = 10,
                        maxit = 20,
                        pred = quickpred(select_if(filtered_tr, is.numeric),
                                         minpuc = .2,
                                         mincor = .01), 
                        print = FALSE,
                        seed = 1942)

plot(imputed_data_tr, layout = c(2, 1))

#Preform PCA
pca_bg <- prcomp(x = complete(imputed_data_bg, 1),
                 rank. = 35,
                 center = TRUE,
                 scale. = TRUE)

summary(pca_bg)

num_centers <- seq(from = 2, to = 15, by = 1)
tot_withinss_bg <- rep(0, length(num_centers))

for (i in 1:length(num_centers)) {
  kmeans_model_bg <- kmeans(pca_bg$x, num_centers[i])
  
  tot_withinss_bg[i] <- kmeans_model_bg$tot.withinss
}

plot(num_centers, tot_withinss_bg)

kmeans_five_bg <- kmeans(pca_bg$x, 5)
center_info_bg <- data.frame(kmeans_five_bg$centers)
kmeans_five_bg$size

t(pca_bg$rotation) %*% as.matrix(center_info_bg)


scaled_scale_tr <- attr(scaled_tract$total_populationE, 'scaled:scale')
scaled_center_tr <- attr(scaled_tract$total_populationE, 'scaled:center')
unscaled_centers_tr <- center_info_tr

for (i in 1:ncol(center_info_tr)) {
  name_of_column <- colnames(center_info_tr)[i]
  
  unscaled_centers_tr[, name_of_column] <- center_info_tr[, name_of_column] * scaled_scale_tr[name_of_column] + scaled_center_tr[name_of_column]
}

scaled_scale_bg <- attr(scaled_bg$total_populationE, 'scaled:scale')
scaled_center_bg <- attr(scaled_bg$total_populationE, 'scaled:center')
unscaled_centers_bg <- center_info_bg

for (i in 1:ncol(center_info_bg)) {
  name_of_column <- colnames(center_info_bg)[i]
  
  unscaled_centers_bg[, name_of_column] <- center_info_bg[, name_of_column] * scaled_scale_bg[name_of_column] + scaled_center_bg[name_of_column]
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
            values = block_group_map_data_bg$Profile)


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
            values = block_group_map_data_tr$Profile)



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

save.image("clusteringWork.RData")
