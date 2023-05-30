# workspace for adding strain-key renaming functionality
# might implement by adding functionality to auto_rename(), which seems neater
# might have to implement a new function, which may necessitate yet another argument :S

# RMUTGRO was assayed as RMs. Rename function should not attempt to rename RMs
#   Perhaps more sophisticated: it should locate places where names already match and
#   report the project? Or report places where no key exists and were not renamed?
# "Warning: in <project>, <oldStrains> were renamed to <newStrains>."

###

library(openxlsx)
library(tidyverse)

load("./data/blanks_2023_04_11.RData")

# get a working/test version of the strain key
testKey <- read.xlsx("./data/The Phenotype Library Project 2023 05 19.xlsx",
          sheet = "Strain ID Key", startRow = 2)

# make the actual key
key <- select(testKey, -c("Mutation", "Assayed.by", "STPTEST", "STPTEST2"))
columnKey <- c(setNames(names(key[2:4]), names(blanksPackage[1:3])), # first three are in same order
            setNames(names(key[5:6]), names(blanksPackage[5:4]))) # last two are swapped >_>
key <- rename(key, any_of(columnKey))

# begin attempt to use the key

# What is the strain column called in the project data (after processODData())?

# auto_rename()

auto_rename <- function(data, testNames){

  # Test: are ALL the name tests present?
  if(all(is.element(testNames, names(data)))){
    # if is.element() returns all TRUE, then all() is TRUE and all (names) are correct
    message("Column names checked and appear correct.")
  } else {
    # begin rename procedure
    message("At least one column name appears incorrect; renaming.")

    # get indices of variants in names(data)
    where <- c()
    for(test in 1:length(testNames)){
      where <- append(where,
                      stringr::str_which(names(data),
                                         stringr::regex(testNames[test], ignore_case = TRUE))
      )
    }

    # Compile list of actual (existing) variant names
    oldNames <- c(names(data)[where])

    # Create renaming key
    key <- setNames(oldNames, testNames)

    # Rename
    data <- dplyr::rename(data, any_of(key))
  }


  return(data)

} # end auto_rename().


