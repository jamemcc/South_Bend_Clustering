homeValueIndex <- read.csv("Zillow Home Value Index - All Homes.csv",
                           stringsAsFactors = F)
homeValueIndex <- homeValueIndex %>%
  filter(State == "IN") %>%
  filter(CountyName == "Saint Joseph County")

tidyValueIndex <- gather(homeValueIndex, key = DateString, value = "HomeValue",
                         -c(RegionID, RegionName, City, State, Metro, CountyName, SizeRank))

tidyValueIndex <- 3

str_sub(tidyValueIndex$DateString[1], start = 2)
as.Date("1996.04.01", "%Y.%m.%d")


neighborhoodRent <- read.csv("Neighborhood Rent Prices-Zillow.csv",
                             stringsAsFactors = F) %>%
  filter(State == "IN") %>%
  filter(CountyName == "Saint Joseph County")

longRentZillow <- neighborhoodRent %>%
  gather(key = DateString,
         value = "ZRI",
         -c(RegionID, RegionName, City, State, Metro, CountyName, SizeRank))

library(lubridate)
library(scales)

tidyRentZillow <- longRentZillow %>%
  mutate(DateString = str_sub(DateString, start = 2)) %>%
  mutate(Date = as.Date(paste0(DateString, ".1"), "%Y.%m.%d") + months(1) - days(1))

ggplot(
  data = tidyRentZillow,
  aes(x = Date, y = ZRI, color = RegionName)
) + geom_point() + scale_y_continuous(labels = dollar_format())
