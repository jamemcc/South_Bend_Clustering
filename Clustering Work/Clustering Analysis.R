#Set the working directory
setwd("~/GitHub/DS-Now-Final-Project/Final Deliverables")

#Load the necessary libraries
library(tidycensus)
library(dplyr)
library(stringr)
library(leaflet)
library(mice)
library(MissMech)

#Load the data
load("census_data.RData")

#Get rid of margins of error for now
minus_margins_tract <- st_joes_tract %>%
  select(-ends_with('M', ignore.case = F))

#Get rid of margins of error for now
minus_margins_block <- st_joes_block %>%
  select(-ends_with('M', ignore.case = F))

#Find the number of missing values for each dataset
tract_nas <- sapply(minus_margins_tract, function(x){sum(is.na(x))})
block_nas <- sapply(minus_margins_block, function(x){sum(is.na(x))})

#Missing data will be imputed for variables with less that 10% missing
missing_data_cutoff_block <- round(nrow(st_joes_block) * .1, 0)
missing_data_cutoff_tract <- round(nrow(st_joes_tract) * .1, 0)

#Find which variables will be kept after imputation
block_vars <- names(block_nas[block_nas < missing_data_cutoff_block])
tract_vars <- names(tract_nas[tract_nas < missing_data_cutoff_tract])

#Also eliminate vars with only one value
block_unique <- sapply(minus_margins_block, function(x){length(unique(x))})
tract_unique <- sapply(minus_margins_tract, function(x){length(unique(x))})

#Filter columns with less than required NA's or 1 unique value
filtered_block <- minus_margins_block[, block_nas < missing_data_cutoff_block &
                                        block_unique > 1]

#For the tracts, limit to what is in the block groups
filtered_tract <- minus_margins_tract[, tract_nas < missing_data_cutoff_tract &
                                        tract_unique > 3]

#Impute missing data
imputed_data_block <- mice(select_if(filtered_block, is.numeric),
                           m = 10,
                           maxit = 20,
                           pred = quickpred(select_if(filtered_block, is.numeric),
                                            minpuc = .2,
                                            mincor = .01), 
                           print = FALSE,
                           seed = 300)

#Plot imputation to make sure that it was random
plot(imputed_data_block, layout = c(2, 1))

#Impute missing data
imputed_data_tract <- mice(select_if(filtered_tract, is.numeric),
                           m = 1,
                           maxit = 20,
                           pred = quickpred(select_if(filtered_tract, is.numeric),
                                            minpuc = .2,
                                            mincor = .01), 
                           print = FALSE,
                           seed = 122)

#Plot imputation to make sure that it was random
plot(imputed_data_tract, layout = c(2, 1))

#Set a seed for reproducability
set.seed(500)

#Preform principal component analysis
pca_block <- prcomp(x = complete(imputed_data_block, 1),
                    rank. = 35,
                    center = TRUE,
                    scale. = TRUE)

#Look at the summary of the PCA
summary(pca_block)

#Preform PCA
set.seed(500)
pca_tract <- prcomp(x = complete(imputed_data_tract, 1),
                    rank. = 21,
                    center = TRUE,
                    scale. = TRUE)

#Look at the summary of the PCA
summary(pca_tract)

#Create a vector for the number of centers
num_centers <- seq(from = 2, to = 15, by = 1)

#Create empty vectors for the tot_withinss
tot_withinss_block <- rep(0, length(num_centers))
tot_withinss_tract <- rep(0, length(num_centers))

#Compute kmeans different number of centers
for (i in 1:length(num_centers)) {
  
  #Compute kmeans for block groups
  set.seed(500)
  kmeans_model_block <- kmeans(pca_block$x, num_centers[i])
  
  #Record tot_withinss
  tot_withinss_block[i] <- kmeans_model_block$tot.withinss
  
  #Compute kmeans for census tracts
  set.seed(500)
  kmeans_model_tract <- kmeans(pca_tract$x, num_centers[i])
  
  #Record tot_withinss
  tot_withinss_tract[i] <- kmeans_model_tract$tot.withinss
}

#Plot the values to determine
plot(num_centers, tot_withinss_block)
plot(num_centers, tot_withinss_tract)

#Set number of clusters to 5
set.seed(500)
kmeans_five_block <- kmeans(pca_block$x, 5)
#Look at the number of block groups in each cluster
kmeans_five_block$size

#Set number of clusters to 5
set.seed(500)
kmeans_five_tract <- kmeans(pca_tract$x, 5)
#Look at the number of tract in each cluster
kmeans_five_tract$size

#Add the cluster numbers back to the unscaled data
minus_margins_block$cluster <- kmeans_five_block$cluster
minus_margins_tract$cluster <- kmeans_five_tract$cluster

#Use imputed data for unscaled clustes
#First get completed data from MICE, then add names and clusters
imputed_block <- complete(imputed_data_block, 1)
imputed_block <- imputed_block %>%
  mutate(`Block Group` = filtered_block$`Block Group`,
         `Census Tract` = filtered_block$`Census Tract`,
         cluster = kmeans_five_block$cluster) %>%
  select(`Block Group`, `Census Tract`, cluster, everything())

imputed_tract <- complete(imputed_data_tract, 1)
imputed_tract <- imputed_tract %>%
  mutate(`Census Tract` = filtered_tract$`Census Tract`,
         cluster = kmeans_five_tract$cluster) %>%
  select(`Census Tract`, cluster, everything())

#Compute the unscaled center clusters for the block groups
#Imputed data will be used for variables that were imputed
#NA removed data will be used for variables that were
#Not included
unscaled_centers_block <- minus_margins_block %>%
  select(-c(colnames(imputed_block)[colnames(imputed_block) != "cluster"])) %>%
  select_if(is.numeric) %>%
  group_by(cluster) %>%
  summarise_all(mean, na.rm = T)

unscaled_centers_block <- imputed_block %>%
  select_if(is.numeric) %>%
  group_by(cluster) %>%
  summarise_all(mean) %>%
  inner_join(unscaled_centers_block)

#Compute the unscaled center clusters for the block groups
#Imputed data will be used for variables that were imputed
#NA removed data will be used for variables that were
#Not included
unscaled_centers_tract <- minus_margins_tract %>%
  select(-c(colnames(imputed_tract)[colnames(imputed_tract) != "cluster"])) %>%
  select_if(is.numeric) %>%
  group_by(cluster) %>%
  summarise_all(mean, na.rm = T)

unscaled_centers_tract <- imputed_tract %>%
  select_if(is.numeric) %>%
  group_by(cluster) %>%
  summarise_all(mean) %>%
  inner_join(unscaled_centers_tract)

#Save the selected data that will be used for analysis
save(unscaled_centers_block, unscaled_centers_tract,
     minus_margins_block, minus_margins_tract,
     imputed_block, imputed_tract,
     file = "cluster data for analysis.Rdata")

#Save all the data, just in case
save.image("clustering_total_work.RData")

### Get distances from each observation to the cluster mean (centroid) and save
# Combine pca values with cluster ID and GEOID
pca_clusters <- data.frame(cluster = kmeans_five_block$cluster,
                           GEOID = minus_margins_block$GEOID,
                           pca_block$x,
                           stringsAsFactors = F)

# Find distance from center of each cluster
distance_list = list()
for(cluster in 1:5){
  # Combine cluster center with all block groups from that cluster
  cluster_mask <- pca_clusters$cluster == cluster
  temp_cluster_with_center <- rbind(as.data.frame(kmeans_five_block$centers)[cluster,], 
                                    pca_clusters[cluster_mask, -c(1:2)])
  distance_list[[cluster]] <- data.frame(GEOID = pca_clusters[cluster_mask, 'GEOID'], 
                                         distance = as.matrix(dist(temp_cluster_with_center))[-1,1])
}
# Save data
cluster_distances <- rbind_list(distance_list)
save(cluster_distances, file = "cluster_distances.Rdata")
