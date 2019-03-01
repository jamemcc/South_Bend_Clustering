### Change to your directory
setwd("C:/Users/kost1/Documents/GitHub/DS-Now-Final-Project")

library(dplyr)
library(tidycensus)

load("EDA Data.RData")

blockGroupShapes <- stJoesBlockGroup %>%
  select(NAME, geometry)

tractShapes <- stJoesTract %>%
  select(NAME, geometry)

save(blockGroupShapes, tractShapes, file = "census_shapefiles.RData")
rm(list = ls())

censusVars <- c(
  total_population = "B01003_001",
  total_hous_units = "B25001_001",
  total_occ_hous_units = "B25002_002",
  total_vac_hous_units = "B25002_003",
  total_own_occ_units = "B25003_002",
  total_rent_occ_units = "B25003_003",
  total_for_rent_units = "B25004_002",
  total_rented_non_occ = "B25004_003",
  total_for_sale_units = "B25004_004",
  total_sold_non_occ_units = "B25004_005",
  med_rent_over_income = "B25071_001",
  owner_hous_val_25th_per = "B25076_001",
  owner_hous_val_50th_per = "B25077_001",
  owner_hous_val_75th_per = "B25078_001",
  med_owner_cost_mortg = "B25088_002",
  med_owner_cost_no_mortg = "B25088_003",
  med_cost_over_income_mortg = "B25092_002",
  med_cost_over_income_no_mortg = "B25092_003",
  med_hous_cost = "B25105_001",
  med_fam_income = "B19113_001",
  med_rent_0_bed = "B25031_002",
  med_rent_1_bed = "B25031_003",
  med_rent_2_bed = "B25031_004",
  med_rent_3_bed = "B25031_005",
  med_rent_4_bed = "B25031_006",
  med_rent_5_plus_bed = "B25031_007",
  med_rent = "B25064_001"
)

load_variables(2017, "acs5", cache = TRUE)

acs2017Variables <- load_variables(2017, "acs5", cache = TRUE)

pulled_vars <- data.frame(censusVars, stringsAsFactors = F) %>%
  inner_join(acs2017Variables, by = c("censusVars" = "name"))

write.csv(pulled_vars, "Pulled_Variables.csv", row.names = F)

stJoesTract <- get_acs(
  geography = "tract",
  variables = censusVars,
  state = "IN",
  county = "St. Joseph County",
  output = "wide",
  geometry = FALSE
)


stJoesBlockGroup <- get_acs(
  geography = "block group",
  variables = censusVars,
  state = "IN",
  county = "St. Joseph County",
  output = "wide",
  geometry = FALSE
)

write.csv(stJoesTract, "St_Joes_Tract_Data.csv", row.names = F)
write.csv(stJoesBlockGroup, "St_Joes_Block_Group_Data.csv", row.names = F)
