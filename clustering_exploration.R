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

####### Some profile Exploration Work from Dan

# use the unscaled_centers_bg dataframe to plot some interesting variables

# Median Age
ggplot(
  data = unscaled_centers_bg,
  aes(x = cluster, y = median_ageE)
) + geom_bar(stat = "identity") +
  labs(y = "Median Age",
       x = "Cluster",
       title = "Median Age per Cluster")

# Maybe Median Age looks better as a boxplot?  Need to use minus_margins_bg dataframe for this
ggplot(
  data = minus_margins_bg,
  aes(x = as.factor(cluster), y = median_ageE)
) + geom_boxplot() +
  labs(y = "Median Age",
       x = "Cluster",
       title = "Median Age per Cluster")

# Next lets look at Race, first by raw numbers
library(reshape2)
race_melted <- melt(minus_margins_bg, id.vars = "cluster", measure.vars = c("tot_whiteE", "tot_blackE",
                                                                            "tot_asianE", "tot_nativeE",
                                                                            "tot_pacificE", "tot_otherE"))
# replace values of races
levels(race_melted$variable)[levels(race_melted$variable) == "tot_whiteE"] <- "White"
levels(race_melted$variable)[levels(race_melted$variable) == "tot_blackE"] <- "Black"
levels(race_melted$variable)[levels(race_melted$variable) == "tot_asianE"] <- "Asian"
levels(race_melted$variable)[levels(race_melted$variable) == "tot_nativeE"] <- "Native Amer"
levels(race_melted$variable)[levels(race_melted$variable) == "tot_pacificE"] <- "Pacific"
levels(race_melted$variable)[levels(race_melted$variable) == "tot_otherE"] <- "Other"

# population by Race bar chart
ggplot(
  data = race_melted,
  aes(x = as.factor(cluster), y = value, fill = variable)
) + geom_bar(stat = "identity") +
  labs(fill = "Race",
       y = "Population",
       x = "Cluster",
       title = "Population by Race") +
  scale_fill_discrete(labels = c("White", "Black", "Asian", "Native Amer", "Pacific Isl", "Other"))


# bar plot for a single cluster
ggplot(grouped_race_by_cluster1, aes(x = factor(cluster), y = value/sum(value) * 100, fill = factor(variable))) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(x = "Cluster", y = "percent", fill = "Race") +
  theme_minimal(base_size = 14) + 
  scale_fill_discrete(labels = c("White", "Black", "Asian", "Native Amer", "Pacific Isl", "Other"))


# group by cluster and race to create a different bar chart view
grouped_race_by_cluster <- race_melted %>% 
  group_by(cluster, variable) %>% summarise(value = sum(value))

# lets try a bar plot
ggplot(grouped_race_by_cluster, aes(x = factor(cluster), y = value/sum(value) * 100, fill = factor(variable))) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(x = "Cluster", y = "percent", fill = "Race") +
  theme_minimal(base_size = 14)

# look at some aspects of housing - this has some really good differentiation in it.  We can add lots more
# calculations in here to highlight differences.  Thinking this can become some tables in a final report
housing_by_cluster <- minus_margins_bg %>% group_by(cluster) %>%
  summarise(total_pop = sum(total_populationE), total_housing_units = sum(total_hous_unitsE),
            people_per_total_unit = sum(total_populationE)/sum(total_hous_unitsE),
            percent_vacant = sum(total_vac_hous_unitsE)/sum(total_hous_unitsE),
            rent_owner_occupied_ratio = sum(total_rent_occ_unitsE)/ sum(total_own_occ_unitsE),
            average_house_value = sum(owner_hous_val_50th_perE, na.rm = TRUE)/n(),
            med_family_income = sum(med_fam_incomeE, na.rm = TRUE)/n(),
            med_rent = sum(med_rentE, na.rm = TRUE)/n())

# lets look at education levels
education_melted <- melt(minus_margins_bg, id.vars = "cluster", measure.vars = c("no_schooling_completedE", 
                                                                                 "nursery_schoolE",
                                                                            "kindergartenE", "first_gradeE",
                                                                            "second_gradeE", "third_gradeE",
                                                                            "fourth_gradeE", "fifth_gradeE",
                                                                            "sixth_gradeE", "seventh_gradeE",
                                                                            "eigth_gradeE", "ninth_gradeE",
                                                                            "tenth_gradeE", "eleventh_gradeE",
                                                                            "twelfth_grade_no_diplomaE",
                                                                            "regular_high_school_diplomaE",
                                                                            "ged_or_alternative_credentialE",
                                                                            "some_college_less_than_1_yearE",
                                                                            "some_college_1_or_more_years_no_degreeE",
                                                                            "associates_degreeE", "bachelors_degreeE",
                                                                            "masters_degreeE", 
                                                                            "professional_school_degreeE",
                                                                            "doctorate_degreeE"))

# classify each level of education into a grouping of Below Elementary, Elementary, High School, 
# Some College, Bachelors Degree, Masters or Above
# Will try to align with highest level achieved (i.e. 10th grade would be Elementary)
education_melted <- education_melted %>% mutate(education_level = ifelse(variable %in% c("no_schooling_completedE", 
                                                                                         "nursery_schoolE",
                                                                                         "kindergartenE", "first_gradeE",
                                                                                         "second_gradeE", "third_gradeE",
                                                                                         "fourth_gradeE", "fifth_gradeE",
                                                                                         "sixth_gradeE", "seventh_gradeE"), 
                                                                         "Below Elementary",
                                                                         ifelse(variable %in% c("eigth_gradeE", "ninth_gradeE",
                                                                                                "tenth_gradeE", "eleventh_gradeE"),
                                                                                "Elementary",
                                                                         ifelse(variable %in% c("twelfth_grade_no_diplomaE",
                                                                                                "regular_high_school_diplomaE",
                                                                                                "ged_or_alternative_credentialE"),
                                                                                "High School",
                                                                         ifelse(variable %in% c("some_college_less_than_1_yearE",
                                                                                                "some_college_1_or_more_years_no_degreeE",
                                                                                                "associates_degreeE"),
                                                                                "Some College",
                                                                          ifelse(variable %in% c("bachelors_degreeE"),
                                                                                 "Bachelors Degree",
                                                                          ifelse(variable %in% c("masters_degreeE", 
                                                                                                 "professional_school_degreeE",
                                                                                                 "doctorate_degreeE"),
                                                                                 "Masters or Above", "Unknown")))))))

# calcuate the percent achieved for each cluster
education_by_cluster <- education_melted %>% group_by(cluster, education_level) %>% 
  summarise(total = sum(value))

# tried to do this table, but something is wrong in the way it is calculating by group
education_table <- education_melted %>% 
  count(cluster, education_level) %>%
  group_by(cluster) %>%
  mutate(prop = n/sum(n)) %>%
  select(-n) %>%
  spread(key = education_level, value = prop)



####ASHLEY

ggplot(
  data = minus_margins_bg,
  aes(x = total_hous_unitsE, y = median_ageE, color = as.factor(cluster))
) + geom_point() +
  labs(color = "Profile",
       x = "Housing Units",
       y = "Median Age",
       title = "Age vs Housing Units")


ggplot(
  data = minus_margins_bg,
  aes(x = total_hous_unitsE, y = tot_whiteE, color = as.factor(cluster))
) + geom_point() +
  labs(color = "Profile",
       x = "Housing Units",
       y = "Total White",
       title = "Total White vs Housing Units")

ggplot(
  data = minus_margins_bg,
  aes(x = total_hous_unitsE, y = tot_blackE, color = as.factor(cluster))
) + geom_point() +
  labs(color = "Profile",
       x = "Housing Units",
       y = "Total Black",
       title = "Total Black vs Housing Units")

library(dplyr)

#find median ages
minus_margins_bg %>% 
  count(cluster, median_ageE) %>%
  group_by(as.factor(cluster)) %>%
  summarise(median_age = median(median_ageE))


#find total population
minus_margins_bg %>% 
  count(cluster, total_populationE) %>%
  group_by(as.factor(cluster)) %>%
  summarise(total_pop = sum(total_populationE))


#race for each cluster
# Maybe interesting to see each Cluster with Race as a pie chart (just to make the storytelling person mad)
# group the melted race data by cluster 1 for easier use

# set the margins - had to use a big top margin because the plot labels were getting cut-off
m = list(
  l = 40,
  r = 40,
  b = 50,
  t = 150,
  pad = 0
)

# cluster 1
grouped_race_by_cluster1 <- race_melted %>% filter(cluster == 1) %>% 
  group_by(cluster, variable) %>% summarise(value = sum(value))

# ggplot pie chart for cluster 1
grouped_race_by_cluster1 %>% 
  ggplot(
    aes(x = as.factor(cluster), y = value, fill = variable)
  ) + geom_bar(stat = "identity") + coord_polar("y") +
  geom_text(aes(label = paste0(round(value/sum(value)*100,1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Race",
       y = NULL,
       x = NULL,
       title = "Cluster 1 by Race") +
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank()) +
  scale_fill_discrete(labels = c("White", "Black", "Asian", "Native Amer", "Pacific Isl", "Other"))

# cluster 1 pie plot using plotly library
library(plotly)
plot_ly(grouped_race_by_cluster1, labels = ~variable, values = ~value, type = "pie",
        textposition = "outside", textinfo = "label+percent", showlegend = FALSE,
        width = 700, height = 700) %>%
  layout(title = "Cluster 1 by Race",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         margin = m)

#cluster 2
grouped_race_by_cluster2 <- race_melted %>% filter(cluster == 2) %>% 
  group_by(cluster, variable) %>% summarise(value = sum(value))

plot_ly(grouped_race_by_cluster2, labels = ~variable, values = ~value, type = "pie",
        textposition = "outside", textinfo = "label+percent", showlegend = FALSE, width = 700,
        height = 700) %>%
  layout(title = "Cluster 2 by Race",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         autosize = FALSE,
         margin = m)

#cluster 3
grouped_race_by_cluster3 <- race_melted %>% filter(cluster == 3) %>% 
  group_by(cluster, variable) %>% summarise(value = sum(value))

plot_ly(grouped_race_by_cluster3, labels = ~variable, values = ~value, type = "pie",
        textposition = "outside", textinfo = "label+percent", showlegend = FALSE,
        width = 700, height = 700) %>%
  layout(title = "Cluster 3 by Race",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        margin = m)

#cluster 4
grouped_race_by_cluster4 <- race_melted %>% filter(cluster == 4) %>% 
  group_by(cluster, variable) %>% summarise(value = sum(value))

plot_ly(grouped_race_by_cluster4, labels = ~variable, values = ~value, type = "pie",
        textposition = "outside", textinfo = "label+percent", showlegend = FALSE,
        width = 700, height = 700) %>%
  layout(title = "Cluster 4 by Race",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         margin = m)

#cluster 5
grouped_race_by_cluster5 <- race_melted %>% filter(cluster == 5) %>% 
  group_by(cluster, variable) %>% summarise(value = sum(value))

plot_ly(grouped_race_by_cluster5, labels = ~variable, values = ~value, type = "pie",
        textposition = "outside", textinfo = "label+percent", showlegend = FALSE,
        width = 700, height = 700) %>%
  layout(title = "Cluster 5 by Race",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         margin = m)


#total housing units
minus_margins_bg %>% 
  count(cluster, total_hous_unitsE) %>%
  group_by(as.factor(cluster)) %>%
  summarise(total_houses = sum(total_hous_unitsE))


#median fam income
minus_margins_bg %>% 
  count(cluster, med_fam_incomeE) %>%
  group_by(as.factor(cluster)) %>%
  summarise(med_family_income = median(med_fam_incomeE, na.rm=TRUE))


#family households vs non-family households
minus_margins_bg %>% 
  count(cluster, tot_householdsE, family_householdsE, non_family_householdsE) %>%
  group_by(as.factor(cluster)) %>%
  summarise(families = sum(family_householdsE/tot_householdsE), nonfamilies = sum(non_family_householdsE/tot_householdsE), 
            totalhouseholds = sum(tot_householdsE))






