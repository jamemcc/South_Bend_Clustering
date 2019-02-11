library(tidycensus)
library(tidyverse)
library(ggmap)
library(viridis)

tidycensus::census_api_key("aca156af3668f91e83aa2ef537451d708acf4838", install = T)

#5-year American Community Survey 2015
#Info Here: https://walkerke.github.io/tidycensus/
acs2015Variables <- load_variables(2016, "acs5", cache = TRUE)

stJoesTract <- get_acs(geography = "tract", 
                       variables = c(totalPopulation = "B01003_001",
                                     rentPortion = "B25072_001",
                                     ownerUnits = "B25106_002",
                                     rentalUnits = "B25106_024"),
                       state = "IN",
                       county = "St. Joseph County",
                       output = "wide")

stJoesTractSpatial <- get_acs(geography = "tract", 
                              variables = c(totalPopulation = "B01003_001",
                                            rentPortion = "B25072_001",
                                            ownerUnits = "B25106_002",
                                            rentalUnits = "B25106_024"),
                              state = "IN",
                              county = "St. Joseph County",
                              geometry = TRUE)

stJoesTractSpatial %>%
  ggplot() + 
  geom_sf()


