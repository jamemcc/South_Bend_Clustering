### Change to your directory
setwd("C:/Users/kost1/Documents/GitHub/DS-Now-Final-Project")

library(dplyr)
library(tidycensus)
library(stringr)

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
  med_non_fam_income = "B19202_001",
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
  family_other_households = "B11001_004",
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
  agg_income_deficit_no_husband = "B17011_005",
  median_household_income = "B25119_001",
  median_hh_income_own = "B25119_002",
  median_hh_income_rent = "B25119_003",
  no_schooling_completed = "B15003_002",
  nursery_school = "B15003_003",
  kindergarten = "B15003_004",
  first_grade = "B15003_005",
  second_grade = "B15003_006",
  third_grade = "B15003_007",
  fourth_grade = "B15003_008",
  fifth_grade = "B15003_009",
  sixth_grade = "B15003_010",
  seventh_grade = "B15003_011",
  eigth_grade = "B15003_012",
  ninth_grade = "B15003_013",
  tenth_grade = "B15003_014",
  eleventh_grade = "B15003_015",
  twelfth_grade_no_diploma = "B15003_016",
  regular_high_school_diploma = "B15003_017",
  ged_or_alternative_credential = "B15003_018",
  some_college_less_than_1_year = "B15003_019",
  some_college_1_or_more_years_no_degree = "B15003_020",
  associates_degree = "B15003_021",
  bachelors_degree = "B15003_022",
  masters_degree = "B15003_023",
  professional_school_degree = "B15003_024",
  doctorate_degree = "B15003_025",
  commute_car_truck_or_van = "B08301_002",
  commute_drove_alone = "B08301_003",
  commute_carpooled = "B08301_004",
  commute_in_2_person_carpool = "B08301_005",
  commute_in_3_person_carpool = "B08301_006",
  commute_in_4_person_carpool = "B08301_007",
  commute_in_5_or_6_person_carpool = "B08301_008",
  commute_in_7_or_more_person_carpool = "B08301_009",
  commute_public_transportation_excluding_taxicab = "B08301_010",
  commute_bus_or_trolley_bus = "B08301_011",
  commute_streetcar_or_trolley_car = "B08301_012",
  commute_subway_or_elevated = "B08301_013",
  commute_railroad = "B08301_014",
  commute_ferryboat = "B08301_015",
  commute_taxicab = "B08301_016",
  commute_motorcycle = "B08301_017",
  commute_bicycle = "B08301_018",
  commute_walked = "B08301_019",
  commute_other_means = "B08301_020",
  commute_worked_at_home = "B08301_021",
  hh_income_less_than_10000 = 'B19001_002',
  hh_income_10000_to_14999 = 'B19001_003',
  hh_income_15000_to_19999 = 'B19001_004',
  hh_income_20000_to_24999 = 'B19001_005',
  hh_income_25000_to_29999 = 'B19001_006',
  hh_income_30000_to_34999 = 'B19001_007',
  hh_income_35000_to_39999 = 'B19001_008',
  hh_income_40000_to_44999 = 'B19001_009',
  hh_income_45000_to_49999 = 'B19001_010',
  hh_income_50000_to_59999 = 'B19001_011',
  hh_income_60000_to_74999 = 'B19001_012',
  hh_income_75000_to_99999 = 'B19001_013',
  hh_income_100000_to_124999 = 'B19001_014',
  hh_income_125000_to_149999 = 'B19001_015',
  hh_income_150000_to_199999 = 'B19001_016',
  hh_income_200_000_or_more = 'B19001_017',
  workers_no_vehicle_available = 'B08014_002',
  workers_1_vehicle_available = 'B08014_003',
  workers_2_vehicles_available = 'B08014_004',
  workers_3_vehicles_available = 'B08014_005',
  workers_4_vehicles_available = 'B08014_006',
  workers_5_plus_vehicles_available = 'B08014_007'
)

acs2018Variables <- load_variables(2018, "acs5", cache = TRUE)

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

#Split NAME into separate columns
stJoesBlockGroup$BlockGroup <- apply(stJoesBlockGroup[, 'NAME'], 1, function(x){str_split(x, pattern = ", ")[[1]][1]})
stJoesBlockGroup$CensusTract <- apply(stJoesBlockGroup[, 'NAME'], 1, function(x){str_split(x, pattern = ", ")[[1]][2]})
stJoesBlockGroup$County <- apply(stJoesBlockGroup[, 'NAME'], 1, function(x){str_split(x, pattern = ", ")[[1]][3]})
stJoesBlockGroup$State <- apply(stJoesBlockGroup[, 'NAME'], 1, function(x){str_split(x, pattern = ", ")[[1]][4]})

#Split NAME into separate columns
stJoesTract$CensusTract <- apply(stJoesTract[, 'NAME'], 1, function(x){str_split(x, pattern = ", ")[[1]][1]})
stJoesTract$County <- apply(stJoesTract[, 'NAME'], 1, function(x){str_split(x, pattern = ", ")[[1]][2]})
stJoesTract$State <- apply(stJoesTract[, 'NAME'], 1, function(x){str_split(x, pattern = ", ")[[1]][3]})


save(stJoesTract, stJoesBlockGroup, file = "census_data.RData")

stJoesTractCSV <- stJoesTract %>%
  select(CensusTract, County, State, everything()) %>%
  select(-ends_with('M', ignore.case = F), -GEOID, -NAME)

colnames(stJoesTractCSV)[4:ncol(stJoesTractCSV)] <- names(censusVars)

stJoesBlockGroupCSV <- stJoesBlockGroup %>%
  select(BlockGroup, CensusTract, County, State, everything()) %>%
  select(-ends_with('M', ignore.case = F), -GEOID, -NAME)

colnames(stJoesBlockGroupCSV)[5:ncol(stJoesBlockGroupCSV)] <- names(censusVars)

write.csv(stJoesTractCSV, "St_Joes_Tract_Data.csv", row.names = F)
write.csv(stJoesBlockGroupCSV, "St_Joes_Block_Group_Data.csv", row.names = F)

stJoesBlockGroup %>%
  summarize(pop = sum(total_populationE))

stJoesTract %>%
  summarize(pop = sum(total_populationE))

pca_bg$x
