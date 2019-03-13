library(stringr)

cenCode <- "B08014_002
B08014_003
B08014_004
B08014_005
B08014_006
B08014_007"

cenLabels <- "No vehicle available 
1 vehicle available 
2 vehicles available 
3 vehicles available 
4 vehicles available 
5 or more vehicles available "

cenCodeSplit <- unlist(str_split(cenCode, pattern = "\n"))

cenLabelsSplit <- unlist(str_split(cenLabels, pattern = "\n"))

cenLabelsClean <- paste0("workers_", cenLabelsSplit)

cenLabelsClean <- make.names(cenLabelsClean)

cenLabelsClean <- tolower(str_replace_all(cenLabelsClean, "[.]", "_"))

cenLabelsClean <- str_replace_all(cenLabelsClean, "__", "_")
cenLabelsClean <- str_replace_all(cenLabelsClean, "__", "_")


print(paste(cenLabelsClean, paste0("'", cenCodeSplit, "'"), sep = " = ", collapse = ", "))
