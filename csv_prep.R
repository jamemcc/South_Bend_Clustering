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
  med_rent = "B25064_001",
  tot_white = "B02001_002",
  tot_black = "B02001_003",
  tot_native = "B02001_004",
  tot_asian = "B02001_005",
  tot_pacific = "B02001_006",
  tot_other = "B02001_007",
  tot_two_plus = "B02001_008",
  median_age = "B01002_001",
  median_male_age = "B01002_002",
  median_female_age = "B01002_003",
  tot_households = "B11001_001",
  family_households = "B11001_002",
  family_married_households = "B11001_003",
  family_other_households = "B11001_003",
  non_family_households = "B11001_007",
  non_family_households_alone = "B11001_008",
  non_family_households_not_alone = "B11001_009",
  two_person_family = "B11016_003",
  three_person_family = "B11016_004",
  four_person_family = "B11016_005",
  five_person_family = "B11016_006",
  six_person_family = "B11016_007",
  seven_plus_person_family = "B11016_008",
  one_person_nonfamily = "B11016_010",
  two_person_nonfamily = "B11016_011",
  three_person_nonfamily = "B11016_012",
  four_person_nonfamily = "B11016_013",
  five_person_nonfamily = "B11016_014",
  six_person_nonfamily = "B11016_015",
  seven_plus_person_nonfamily = "B11016_016",
  labor_force = "B23025_002",
  civilian_labor_force = "B23025_003",
  civilian_labor_force_employ = "B23025_004",
  civilian_labor_force_unemploy = "B23025_005",
  military_labor_force = "B23025_006",
  non_labor_force = "B23025_007",
  agg_income_deficit = "B17011_001",
  agg_income_deficit_married = "B17011_002",
  agg_income_deficit_nonmarried = "B17011_003",
  agg_income_deficit_no_wife = "B17011_004",
  agg_income_deficit_no_husband = "B17011_005"
)

acs2017Variables <- load_variables(2017, "acs5", cache = TRUE)

pulled_vars <- data.frame(censusVars, stringsAsFactors = F) %>%
  inner_join(acs2017Variables, by = c("censusVars" = "name"))

pulled_vars$variable_name <- names(censusVars)

pulled_vars <- pulled_vars %>%
  select(variable_name, everything())

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

save(stJoesTract, stJoesBlockGroup, file = "total_census_data.RData")

write.csv(stJoesTract, "St_Joes_Tract_Data.csv", row.names = F)
write.csv(stJoesBlockGroup, "St_Joes_Block_Group_Data.csv", row.names = F)
