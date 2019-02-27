library(tidycensus)
library(dplyr)
library(stringr)

censusVars <- c(
  `Total Population` = "B01003_001",
  `Total Housing Units` = "B25001_001",
  `Total Occupied Housing Units` = "B25002_002",
  `Total Vacant Housing Units` = "B25002_003",
  `Total Owner-Occupied Units` = "B25003_002",
  `Total Renter-Occupied Units` = "B25003_003",
  `Total For-Rent Units` = "B25004_002",
  `Total Rented Non-Occ Units` = "B25004_003",
  `Total For-Sale Units` = "B25004_004",
  `Total Sold Non-Occ Units` = "B25004_005",
  `Med Rent/Income%` = "B25071_001",
  `25% Owner Occ Housing Val` = "B25076_001",
  `50% Owner Occ Housing Val` = "B25077_001",
  `75% Owner Occ Housing Val` = "B25078_001",
  `Med Owner Costs-Mortgage` = "B25088_002",
  `Med Owner Costs-No Mortgage` = "B25088_003",
  `Med Costs/Income%-Mortgage` = "B25092_002",
  `Med Costs/Income%-No Mortgage` = "B25092_003",
  `Med Monthly Housing Costs` = "B25105_001",
  `Med Family Income` = "B19113_001",
  `Med Rent-No Bedrooms` = "B25031_002",
  `Med Rent-1 Bedroom` = "B25031_003",
  `Med Rent-2 Bedrooms` = "B25031_004",
  `Med Rent-3 Bedrooms` = "B25031_005",
  `Med Rent-4 Bedrooms` = "B25031_006",
  `Med Rent-5+ Bedrooms` = "B25031_007",
  `Med Rent` = "B25064_001"
)

#tidycensus::census_api_key("aca156af3668f91e83aa2ef537451d708acf4838", install = T)

stJoesBlockGroup <- get_acs(
  geography = "block group",
  variables = censusVars,
  state = "IN",
  county = "St. Joseph County",
  output = "wide",
  geometry = TRUE
)

#Split NAME into separate columns
stJoesBlockGroup$BlockGroup <- apply(stJoesBlockGroup[, 'NAME'], 1, function(x){str_split(x, pattern = ", ")[[1]][1]})
stJoesBlockGroup$CensusTract <- apply(stJoesBlockGroup[, 'NAME'], 1, function(x){str_split(x, pattern = ", ")[[1]][2]})
stJoesBlockGroup$County <- apply(stJoesBlockGroup[, 'NAME'], 1, function(x){str_split(x, pattern = ", ")[[1]][3]})
stJoesBlockGroup$State <- apply(stJoesBlockGroup[, 'NAME'], 1, function(x){str_split(x, pattern = ", ")[[1]][4]})

stJoesTract <- get_acs(
  geography = "tract",
  variables = censusVars,
  state = "IN",
  county = "St. Joseph County",
  output = "wide",
  geometry = TRUE
)

#Split NAME into separate columns
stJoesTract$CensusTract <- apply(stJoesTract[, 'NAME'], 1, function(x){str_split(x, pattern = ", ")[[1]][1]})
stJoesTract$County <- apply(stJoesTract[, 'NAME'], 1, function(x){str_split(x, pattern = ", ")[[1]][2]})
stJoesTract$State <- apply(stJoesTract[, 'NAME'], 1, function(x){str_split(x, pattern = ", ")[[1]][3]})

#Save the data
save(stJoesBlockGroup, stJoesTract, file = "census_data.RData")

#Load the data
load("census_data.RData")

#Get rid of margins of error for now
minus_margins_tr <- stJoesTract %>%
  select(-ends_with('M', ignore.case = F))

minus_margins_tr <- minus_margins_tr %>%
  select(-geometry) %>%
  as_tibble() %>%
  select(-geometry) %>%
  as_tibble()

minus_margins_bg <- stJoesBlockGroup %>%
  select(-ends_with('M', ignore.case = F))

minus_margins_bg <- minus_margins_bg %>%
  select(-geometry) %>%
  as_tibble() %>%
  select(-geometry) %>%
  as_tibble()

#Find the number of missing values for each dataset
sapply(minus_margins_tr, function(x){sum(is.na(x))})
sapply(minus_margins_bg, function(x){sum(is.na(x))})

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
