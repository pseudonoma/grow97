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
load("./data/growthData_2023_04_11.RData")

### Set up the actual key ###
# get a working/test version of the strain key
testKey <- read.xlsx("./data/The Phenotype Library Project 2023 05 19.xlsx",
          sheet = "Strain ID Key", startRow = 2)

# make the actual key by dropping cols and changing names
key <- select(testKey, -c("Mutation", "Assayed.by", "STPTEST", "STPTEST2"))
columnKey <- c(setNames(names(key[2:4]), names(blanksPackage[1:3])), # first three are in same order
            setNames(names(key[5:6]), names(blanksPackage[5:4]))) # last two are swapped >_>
key <- rename(key, any_of(columnKey))

###

# for every row in [current data frame],
#   if [Strain] value != [key] value
#   and [key] value != NA
#   [Strain] value <- [key] value
# OR
# for every row in [current data frame],
#   [Strain] value <- [key] value that != NA
# OR
# replace all instances in column <Strain> with values in <key> that matches
#

# debug
folders <- names(blanksPackage)
project <- 5
folders[project]

# auto_rename()
auto_rename <- function(data, testNames){

  ### 1. Check column values
  # define the column to change
  targetCol <- names(key[1])
  message(paste0("Key detected. Checking values in column ", targetCol, ": \n"))

  # extract a sub-key containing only reference and relevant variant column
  subKey <- key |>
    dplyr::select(all_of(targetCol), folders[project])|> # reference is hardcoded, variant is relative
    # dplyr::filter(!is.na(folders[project])) # doesn't seem to work on colnames?
    # dplyr::filter(complete.cases(.)) # this doesn't work either, people be lyin on the internet
    na.omit()

  # construct testcase:
  # testData <- dataPackage[[project]][["processed_data"]]

  # construct vector for replacement
  replacements <- subKey[ ,targetCol][match(data[[targetCol]], subKey[ ,2])] # new values
  originals <- testData[,targetCol][!is.na(replacements)] # old values

  # replace elements of testData$`targetCol` that match non-NA replacements
  data[[targetCol]][!is.na(replacements)] <- replacements[!is.na(replacements)]

  message(paste(unique(originals[!is.na(originals)]),
                "renamed", unique(replacements[!is.na(replacements)]), "\n"))


  ### 2. Check column names
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

# key matching testcase

testData <- data.frame("Strain" = c(NA, "Var1", "Var2", "RM03", "Var4", "Var5", "RM06"))
subKey <- data.frame("Strain" = c("RM01", "RM02", "RM03", "RM04", "RM05", "RM06", "RM-extra"),
                     "Variant" = c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var-extra"))


replacements <- subKey[ ,targetCol][match(testData[ ,targetCol], subKey[ ,2])]
originals <- testData[,targetCol][!is.na(replacements)]
testData[,targetCol][!is.na(replacements)] <- replacements[!is.na(replacements)]

message(paste0("Renaming column ", targetCol, ": \n"))
message(paste(unique(originals[!is.na(originals)]),
              "renamed", unique(replacements[!is.na(replacements)]), "\n"))


indices <- match(data[,targetCol], key[, 2])
replacements <- key$Strain[indices]

data[!is.na(replacements), targetCol] <- replacements[!is.na(replacements)]



#

key <- data.frame(A = 1:3, B = LETTERS[1:3])

test <- c(2, 5, 4, 1)

swaps <- key$B[match(test, key$A)]
swaps

test[!is.na(swaps)] <- swaps[!is.na(swaps)]
test
