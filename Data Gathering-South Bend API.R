library(rjson)

json_file <- "https://opendata.arcgis.com/datasets/2636929ba1b945b28965af3762da4af2_1.geojson"

json_data <- fromJSON(file = json_file)

data.frame(unlist(json_data$features[[1]]$properties))