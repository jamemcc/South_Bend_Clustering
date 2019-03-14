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

#Plot imputation to make sure that it was random
plot(imputed_data_bg, layout = c(2, 1))

#Impute missing data
imputed_data_tr <- mice(select_if(filtered_tr, is.numeric),
                        m = 10,
                        maxit = 20,
                        pred = quickpred(select_if(filtered_tr, is.numeric),
                                         minpuc = .2,
                                         mincor = .01), 
                        print = FALSE,
                        seed = 1942)

#Plot imputation to make sure that it was random
plot(imputed_data_tr, layout = c(2, 1))

#Preform PCA
set.seed(500)
pca_bg <- prcomp(x = complete(imputed_data_bg, 1),
                 rank. = 35,
                 center = TRUE,
                 scale. = TRUE)

#Look at the summary of the PCA
summary(pca_bg)

#Preform PCA
set.seed(500)
pca_tr <- prcomp(x = complete(imputed_data_tr, 1),
                 rank. = 21,
                 center = TRUE,
                 scale. = TRUE)

#Look at the summary of the PCA
summary(pca_tr)

#Create a vector for the number of centers
num_centers <- seq(from = 2, to = 15, by = 1)
#Create empty vectors for the tot_withinss
tot_withinss_bg <- rep(0, length(num_centers))
tot_withinss_tr <- rep(0, length(num_centers))

#Compute kmeans different number of centers
for (i in 1:length(num_centers)) {
  
  #Compute kmeans for block groups
  set.seed(500)
  kmeans_model_bg <- kmeans(pca_bg$x, num_centers[i])
  
  #Record tot_withinss
  tot_withinss_bg[i] <- kmeans_model_bg$tot.withinss
  
  #Compute kmeans for census tracts
  set.seed(500)
  kmeans_model_tr <- kmeans(pca_tr$x, num_centers[i])
  
  #Record tot_withinss
  tot_withinss_tr[i] <- kmeans_model_tr$tot.withinss
}

#Plot the values to determine
plot(num_centers, tot_withinss_bg)
plot(num_centers, tot_withinss_tr)

#Set number of clusters to 5
set.seed(500)
kmeans_five_bg <- kmeans(pca_bg$x, 5)
#Look at the number of block groups in each cluster
kmeans_five_bg$size

#Set number of clusters to 5
set.seed(500)
kmeans_five_tr <- kmeans(pca_tr$x, 5)
#Look at the number of tract in each cluster
kmeans_five_tr$size

#Add the cluster numbers back to the unscaled data
minus_margins_bg$cluster <- kmeans_five_bg$cluster
minus_margins_tr$cluster <- kmeans_five_tr$cluster

#Compute the unscaled center clusters for the block groups
unscaled_centers_bg <- minus_margins_bg %>%
  select_if(is.numeric) %>%
  group_by(cluster) %>%
  summarise_all(mean)

#Compute the unscaled center clusters for the tracts
unscaled_centers_tr <- minus_margins_tr %>%
  select_if(is.numeric) %>%
  group_by(cluster) %>%
  summarise_all(mean)

save.image("clustering_work.RData")
