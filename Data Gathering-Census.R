library(tidycensus)
library(tidyverse)
library(ggmap)
library(viridis)
library(leaflet)

tidycensus::census_api_key("aca156af3668f91e83aa2ef537451d708acf4838", install = T)

#5-year American Community Survey 2015
#Info Here: https://walkerke.github.io/tidycensus/
acs2017Variables <- load_variables(2017, "acs5", cache = TRUE)

censusVars <- c(
  total_population = "B01003_001",
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

stJoesTract <- get_acs(
  geography = "tract",
  variables = censusVars,
  state = "IN",
  county = "St. Joseph County",
  output = "wide",
  geometry = TRUE
)


stJoesBlockGroup <- get_acs(
  geography = "block group",
  variables = censusVars,
  state = "IN",
  county = "St. Joseph County",
  output = "wide",
  geometry = TRUE
)

stJoesTract <- sf::st_transform(stJoesTract, 4326)
stJoesBlockGroup <- sf::st_transform(stJoesBlockGroup, 4326)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = stJoesTract,
              color = "black",
              opacity = .6,
              weight = 2,
              fill = FALSE,
              popup = ~NAME,
              group = "Census Tract") %>%
  addPolygons(data = stJoesBlockGroup,
              color = "blue",
              opacity = .6,
              weight = 2,
              fill = FALSE,
              popup = ~NAME,
              group = "Census Block Group") %>%
  addLayersControl(
    overlayGroups = c("Census Tract", "Census Block Group"),
    options = layersControlOptions(collapsed = FALSE)
  )
