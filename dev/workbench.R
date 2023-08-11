# testrun

source("./R/analysis.R")
source("./R/helpers.R")

renameKey <- readRDS("./data/renaming_key_230811.rds")
testKey <- renameKey[[2]]

allBlanks <- extract_blanks(dataHubPath = "./data/testdata/",
                            colNames = c("Drug", "Strain"),
                            variableKey = testKey,
                            meansBy = c("Drug", "Time_min"),
                            combineData = TRUE,
                            fileMethod = "both")
