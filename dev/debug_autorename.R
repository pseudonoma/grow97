# debugging auto_rename()

source("./R/helpers.R")

#####
# extract_blanks()
# for(project in 1:length(folders)){}

# Define project name and path
projectName <- "Honours Growth Assays (2018)"
projectPath <- paste0("./data/", projectName)

message(paste("\nWorking on project", projectName, "now..."))

# Determine the raw file prefix and process data accordingly
#data <- auto_process(projectPath)
#saveRDS(data, file = "./data/debug_data.rds")
data <- readRDS("./data/debug_data.rds")

# Attempt to rename columns and/or replace values in a column
# processedData <- auto_rename(processedData, projectName, colNames, variableKey)
#####

# define args
#data <- processedData
#projectName <- projectName
testNames <- NULL
projectName <- chartr(" ", "_", projectName)
renameKey <- readRDS("./data/renaming_key 2023 06 01.rds")

auto_rename <- function(data, projectName, testNames, renameKey){

  # hacky fix for project names because I'm tired and pissed off
  # a version of this is run before the wrapper functions export the data object
  projectName <- chartr(" ", "_", projectName)

  ### Fix 1: column names ###
  if(!is.null(testNames)){
    message("Checking column names against supplied ones...")

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

  }

  ### Fix 2: values in a column ###
  if(!is.null(renameKey)){

    # define the column to change
    targetCol <- names(renameKey[1])
    message(paste0("Checking values in column ", targetCol, ": \n"))

    # extract a sub-key containing only reference and relevant variant column
    subKey <- renameKey |>
      dplyr::select(all_of(targetCol), # reference is determined by first col of key
                    projectName)|>  # variant is relative to project
      # dplyr::filter(!is.na(folders[project])) # doesn't seem to work on colnames?
      # dplyr::filter(complete.cases(.)) # this doesn't work either, people be lyin on the internet
      na.omit()

    # construct testcase:
    # testData <- dataPackage[[project]][["processed_data"]]

    # construct vector for replacement
    replacements <- subKey[[targetCol]][match(data[[targetCol]], subKey[, 2])] # new values
    originals <- data[[targetCol]][!is.na(replacements)] # old values

    if(all(is.na(replacements))){
      # replacements are all NA, so all() returns TRUE
      message("No valid replacements in this column.")
    } else {
      # preserve the old names as a new column
      data[[paste0(targetCol, "_old")]] <- data[[targetCol]]
      # replace elements of testData$`targetCol` that match non-NA replacements
      data[[targetCol]][!is.na(replacements)] <- replacements[!is.na(replacements)]
      # currently replaces even identical values; not sure if I want to restrict this to
      # only cases where values in replacements don't match target.

      # report changes because of paranoia (maybe this should be a warning?)
      message(paste(unique(originals[!is.na(originals)]),
                    "replaced with", unique(replacements[!is.na(replacements)]), "\n"))
    }
  }


  return(data)

} # end auto_rename().
