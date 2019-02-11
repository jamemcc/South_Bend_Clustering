homeValueIndex <- read.csv("Zillow Home Value Index - All Homes.csv", stringsAsFactors = F)
homeValueIndex <- homeValueIndex %>%
  filter(State == "IN") %>%
  filter(CountyName == "Saint Joseph County")

tidyValueIndex <- gather(homeValueIndex, key = DateString, value = "HomeValue",
                         -c(RegionID, RegionName, City, State, Metro, CountyName, SizeRank))

tidyValueIndex <- 3

str_sub(tidyValueIndex$DateString[1], start = 2)
as.Date("1996.04.01", "%Y.%m.%d")
