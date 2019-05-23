library(tidyverse)

setwd("~/GitHub/DS-Now-Final-Project/Final Deliverables")

load("cluster data for analysis.RData")

# Find average values for each cluster
cluster_summaries <- minus_margins_block[,c(3:134, 145)] %>%
  group_by(cluster) %>%
  summarise_all(mean, na.rm = TRUE)

# find number of NAs for each cluster
cluster_NA <-  minus_margins_block[,c(3:134, 145)] %>%
  group_by(cluster) %>%
  summarise_all(funs(sum(is.na(.))))

# Find number of non-NAs for each cluster
cluster_NotNA <- minus_margins_block[,c(3:134, 145)] %>%
  group_by(cluster) %>%
  summarise_all(funs(sum(!is.na(.))))

# Find percent NA for each cluster
cluster_pNA <- minus_margins_block[,c(3:134, 145)] %>%
  group_by(cluster) %>%
  summarise_all(funs(mean(!is.na(.))))
# save
write.csv(rbind(cluster_summaries, cluster_NA, cluster_NotNA, cluster_pNA), file = "Profile_Summaries.csv")

# Reformat cluster means for combination with block data
colnames(cluster_summaries)[1] <- "NAME"
cluster_summaries$NAME <- c('Profile 1 Average', 'Profile 2 Average', 'Profile 3 Average', 'Profile 4 Average', 'Profile 5 Average')
cluster_summaries$cluster <- rep(0, 5)

# Get means for the entire county
county_means <- data.frame(t(colMeans(minus_margins_block[,c(3:134)], na.rm = T)))
county_means <- cbind(data.frame('NAME' = 'St Joseph County Average'), county_means)
county_means$cluster = 0

blocks_with_summaries <- rbind(minus_margins_block[,c(2:134, 145)], cluster_summaries, county_means)
#### Now add all the percentages


#### Find percentages for relevant variables ####
# people per housing unit
blocks_with_summaries[,'people_per_housing_unit'] <- blocks_with_summaries[,'total_populationE']/blocks_with_summaries[,'total_hous_unitsE']

# For housing units 
#block_percentages <- minus_margins_block[,c('cluster', 'total_hous_unitsE', 'total_populationE', 'tot_householdsE')] 
hous_names_per <- c('total_occ_hous_per', 'total_own_occ_per', 'total_rent_occ_per',
                 'total_for_rent_per', 'total_rented_non_per', 'total_vac_hous_per')
hous_names <- c('total_occ_hous_unitsE', 'total_own_occ_unitsE', 'total_rent_occ_unitsE',
                'total_for_rent_unitsE', 'total_rented_non_occE', 'total_vac_hous_unitsE')

blocks_with_summaries[,hous_names_per] <- blocks_with_summaries[,hous_names]/rep(blocks_with_summaries[,'total_hous_unitsE'], 
                                                                           ncol(blocks_with_summaries[,hous_names]))

# house values
#hous_price_names <- c('owner_hous_val_25th_perE', 'owner_hous_val_50th_perE', 'owner_hous_val_75th_perE')
#block_percentages[,hous_price_names] <- minus_margins_block[,hous_price_names]

# races
race_values <- c('tot_whiteE', 'tot_blackE', 'tot_asianE', 'tot_pacificE', 'tot_otherE', 'tot_two_plusE')
race_per <- c('white_per', 'black_per', 'asian_per', 'pacific_per', 'other_per', 'two_plus_per')

blocks_with_summaries[,race_per] <- blocks_with_summaries[,race_values]/rep(blocks_with_summaries[,'total_populationE'], 
                                                                      ncol(blocks_with_summaries[,race_values]))
# Households
household_values <- c('family_householdsE', 'non_family_householdsE')
household_per <- c('family_households_per', 'non_family_households_per')
blocks_with_summaries[,household_per] <- blocks_with_summaries[,household_values]/rep(blocks_with_summaries[,'tot_householdsE'], 
                                                                      ncol(blocks_with_summaries[,household_values]))

# labor force
labor_values <- c('labor_forceE', '')
labor_per <- c('labor_force_per', 'employed_per', 'unemployed_per', 'non_labor_force_per')

blocks_with_summaries$labor_force_per <- with(blocks_with_summaries, labor_forceE/total_populationE)
blocks_with_summaries$employed_per <- with(blocks_with_summaries, (civilian_labor_force_employE+military_labor_forceE)/labor_forceE)
blocks_with_summaries$unemployed_per <- with(blocks_with_summaries, civilian_labor_force_unemployE/labor_forceE)
blocks_with_summaries$non_labor_force_per <- with(blocks_with_summaries, non_labor_forceE/total_populationE)


## Load cluster distances and combine with block data and cluster summaries                                
load("cluster_distances.RData")

# Load cluster distances and combine with block group data
cluster_distances2 <- cluster_distances %>%
  inner_join(minus_margins_block[,c('GEOID', 'NAME')], by = "GEOID")

# combine block summaries with cluster distances and block group data
blocks_with_summaries <- blocks_with_summaries %>%
  left_join(cluster_distances2[,c('NAME', 'distance', 'GEOID')], by = 'NAME')

# Change name of cluster column to profile
colnames(blocks_with_summaries)[colnames(blocks_with_summaries) == 'cluster'] <- "Profile"

# Save
write.csv(blocks_with_summaries[,-155], file = "Profile_Summaries_with_Blocks.csv")
save(blocks_with_summaries, file = "blocks_with_summaries.Rdata")
