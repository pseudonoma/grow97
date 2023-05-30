#' Analyze grow96-compatible datasets.
#'
#' `quick_analyze()` automatically analyzes any number of individual `grow96`-compatible datasets
#' and outputs processed and/or summarized growth data as dataframes in a list.
#'
#' @details
#' Each project's data files must be contained in a separate folder, and all project folders
#' placed in a single "hub" folder, the path for which is the argument `dataHubPath`.
#' By default, analyzed data is exported to the working directory, as an RData object with filename
#' `growthData_YYYY_MM_DD.RData`. The exported data is a list with as many elements as projects,
#' each element named according to project folder names. Only summarized data ("analyzed_data")
#' is exported by default, but unsummarized data ("processed_data") can also be exported by
#' setting `exportGrowth` to `TRUE`.
#' @details
#' OD reads are blanked by calling `auto_blank()`. If necessary, blank values can be supplied
#' through the argument `blankData`, which is then used to blank OD data if a project does not
#' have valid blank data available. `forceBlanking` can be set to TRUE to force the use of
#' `blankData` regardless of whether blanks are available in a project.
#' ## Combining data
#' If `combineData` is set to `TRUE`, two additional objects are added to the output. "all_growths"
#' combines all growth curve data into one large tibble. This is then used to generate the second
#' object, "all_analyzed", by calling [`grow96::analyseODData`]. This effectively doubles the amount
#' of data contained in the final data object. This may have consequences, I don't know.
#' ## Renaming column names
#' This function can rename slightly inconsistent variable names (such as `Drug` vs. `drugs`).
#' When `colNames` is a character vector of correct column names, it is passed to the helper
#' function [`auto_rename()`] and is used to check and fix incorrect column names. If column
#' joining causes an error somewhere, column names are likely responsible.
#'
#' @param dataHubPath The path of the "hub" folder containing project folders to analyze.
#' @param forceBlanking If `TRUE`, `blankData` is used for blanking regardless of whether
#' blank data exists in a particular project. Defaults to `FALSE`.
#' @param blankData A dataframe/tibble containing blank values. See [grow96::blankODs]
#' for more information.
#' @param colNames A character vector of column names to check and fix if necessary. See Details
#' for more information.
#' @param exportGrowth Defaults to `FALSE`. If `TRUE`, processed growth data will also be exported.
#' @param combineData Defaults to `FALSE`. If `TRUE`, two additional objects of combined data are
#' also produced. See Details for more information.
#' @param fileData Determines how the output is handled. The default value is `save`, which saves
#' the data as an RData file. To only return the data without saving it, use `return`. To do both,
#' use `both`.
#'
#' @examples
#' quick_analyze(dataHubPath,
#'               forceBlanking = FALSE,
#'               blankData = NULL,
#'               colNames = NULL,
#'               exportGrowth = FALSE,
#'               combineData = FALSE,
#'               fileMethod = "save")
#'
#' @return A list of *n* elements, corresponding to *n* projects, each containing a tibble of
#' processed and/or summarized OD data. May include an additional tibble of combined growth data.
#'
#' @export

quick_analyze <- function(dataHubPath,
                          forceBlanking = FALSE,
                          blankData = NULL,
                          colNames = NULL,
                          exportGrowth = FALSE,
                          combineData = FALSE,
                          fileMethod = "save"){

  # Catch invalid value
  if(!fileMethod %in% c("save", "return", "both")){ # fileMethod
    stop("fileMethod is invalid. Values are case-sensitive: \"save\", \"return\", or \"both\".")
  }

  # Parse file path
  folders <- dir(dataHubPath) # i.e., what projects are going to be analyzed?

  # Prepare final export object and begin auto-processing
  dataPackage <- list()
  for(project in 1:length(folders)){
    message(paste("\nWorking on project", folders[project], "now..."))

    # Define project path
    projectPath <- paste0(dataHubPath, "/", folders[project])

    # Determine the raw file prefix and process data accordingly
    processedData <- auto_process(projectPath)
    # Optional: rename columns
    if(!is.null(colNames)){
      message("Checking column names against supplied ones...")
      processedData <- auto_rename(processedData, colNames)
    }
    # Generate QC report and create value indicating if QC detected blank data
    hasBlanks <- auto_QC(processedData, projectPath)
    # Blank the data appropriately
    blankedData <- auto_blank(processedData, forceBlanking, hasBlanks, blankData)
    # Analyze the data
    analyzedData <- grow96::analyseODData(blankedData)

    # Add the data to the appropriate place in the final export list
    dataPackage[[folders[project]]] <- list("processed_data" = blankedData,
                                            "analyzed_data" = analyzedData)

    message(paste("Project", folders[project], "done."))

  }

  # Optional: glue individual processed growth data into a single dataframe "all_growths"
  if(combineData){

    # extract all processed_data into a list for bind_rows
    allGrowth <- list()
    for(project in 1:length(dataPackage)){
      allGrowth[[project]] <- dataPackage[[project]][["processed_data"]]
    }
    combinedGrowths <- dplyr::bind_rows(allGrowth)

    # Rerun analysis on the combined growth data
    message("Analyzing combined data...")
    combinedAnalysis <- grow96::analyseODData(combinedGrowths)

    # Add combined growth data and name it
    dataPackage[[length(dataPackage)+1]] <- combinedGrowths
    names(dataPackage)[length(dataPackage)] <- "all_growths"
    dataPackage[[length(dataPackage)+1]] <- combinedAnalysis
    names(dataPackage)[length(dataPackage)] <- "all_analyzed"

    # Inelegant method to keep file size down if (combineData) but (!exportGrowth)
    if(!exportGrowth){
      for(project in 1:length(dataPackage)){
        dataPackage[[project]][["processed_data"]] <- NULL
      }
    }

  }

  # Rename export object names, removing whitespaces
  names(dataPackage) <- chartr(" ", "_", names(dataPackage))

  # Optional: export data package as RData to root data folder
  if(fileMethod == "save" || fileMethod == "both"){
    save(dataPackage, file = paste0("./growthData_",
                                    format(Sys.Date(), "%Y_%m_%d"), # like "2023_03_28"
                                    ".RData"))
    message(paste0("\nDone. The data has been saved as an RData file in the working directory."))
  } else {
    message("\nfileMethod is not \"save\" so no RData file will be saved.\n")
  }

  message("\nThank you for using grow97. Have a nice day!\n")

  if (fileMethod == "return" || fileMethod == "both") {
    return(dataPackage)
  } else {
    return(invisible())
  }

} # end quick_analyze().


#' Extract blank values from grow96-compatible datasets.
#'
#' This function extracts blank values from any number of `grow96`-compatible datasets, with
#' options for calculating mean blank ODs and combining all blanks into a single large dataframe.
#'
#' @details
#' `extract_blanks()` is designed to generate blank values for use with `quick_analyze()`.
#' See the documentation for [`quick_analyze()`] for information on how to organize project
#' datasets and how the output is formatted.
#' ## Grouping means
#' Mean blank ODs are calculated by aggregating over all projects. Means can be grouped by
#' supplying a character vector to the argument `meansBy`. The vector must contain variables to
#' group by, which is then passed to [`get_mean_blanks()`].  `meansBy` is necessary for
#' generating blank values for use with `quick_analyze()` or [`grow96::blankODs`].
#' ## Renaming column names
#' This function can rename slightly inconsistent variable names (such as `Drug` vs. `drugs`).
#' When `colNames` is a character vector of correct column names, it is passed to the helper
#' function [`auto_rename()`] and is used to check and fix incorrect column names.
#'
#' @importFrom dplyr bind_rows
#'
#' @inheritParams quick_analyze
#' @param meansBy A character vector of variables to group means by. If this argument is `NULL`
#' (the default), no mean blank ODs will be generated. See Details for more information.
#' @param combineData Defaults to `FALSE`. If `TRUE`, an additional dataframe called
#' "all_blanks" will be generated by combining dataframes from all projects. This argument will
#' automatically be `TRUE` if `meansBy` is defined.
#'
#' @examples
#' extract_blanks(dataHubPath,
#'                colNames = NULL,
#'                meansBy = NULL,
#'                combineData = FALSE,
#'                fileMethod = "save")
#'
#' @return A list of tibbles of blank OD data. May include additional tibbles for combined blanks
#' and mean blank ODs depending on function arguments.
#'
#' @export

extract_blanks <- function(dataHubPath,
                           colNames = NULL,
                           meansBy = NULL,
                           combineData = FALSE,
                           fileMethod = "save"){

  # Catch invalid value
  if(!fileMethod %in% c("save", "return", "both")){ # fileMethod
    stop("fileMethod is invalid. Values are case-sensitive: \"save\", \"return\", or \"both\".")
  }

  # Parse file path
  folders <- dir(dataHubPath) # i.e., what projects are going to be analyzed?

  # Prepare final export object and begin auto-processing
  blanksPackage <- list()
  for(project in 1:length(folders)){
    message(paste("\nWorking on project", folders[project], "now..."))

    # Define project path
    projectPath <- paste0(dataHubPath, "/", folders[project])

    # Determine the raw file prefix and process data accordingly
    processedData <- auto_process(projectPath)

    # Optional: rename columns
    if(!is.null(colNames)){
      message("Checking column names against supplied ones...")
      processedData <- auto_rename(processedData, colNames)
    }

    # Extract all rows with BLANK in column <WellType>, and add to the final export list
    blanksOnly <- processedData[which(processedData$WellType == "BLANK"), ]
    blanksPackage[[folders[project]]] <- blanksOnly
    if(nrow(blanksOnly) == 0){ # warn if a project exports no rows.
      warning(paste0(folders[project]," appears to have no valid BLANK values."))
    }

    message(paste("Project", folders[project], "done."))

  }

  # Force combineData if meansBy has values indicating means are needed
  if(!is.null(meansBy) & !combineData){
    message("Mean ODs are requested but combineData = FALSE; forcing combineData = TRUE.")
    combineData <- TRUE
  }

  # Optional: glue individual project outputs into a single dataframe "all_blanks"
  if(combineData){
    combinedBlanks <- dplyr::bind_rows(blanksPackage)

    # Add combined blanks and name it
    blanksPackage[[length(blanksPackage)+1]] <- combinedBlanks
    names(blanksPackage)[length(blanksPackage)] <- "all_blanks"
  }

  # Optional: compute mean ODs across "all_blanks", grouping by <meansBy>
  if(!is.null(meansBy)){
    meanBlankODs <- get_mean_blanks(blanksPackage[["all_blanks"]], meansBy)

    # Add mean blanks and name it
    blanksPackage[[length(blanksPackage)+1]] <- meanBlankODs
    names(blanksPackage)[length(blanksPackage)] <- "mean_blanks"
  }

  # Rename export object names, removing whitespaces
  names(blanksPackage) <- chartr(" ", "_", names(blanksPackage))

  # Optional: export data package as RData to root data folder
  if(fileMethod == "save" || fileMethod == "both"){
    save(blanksPackage, file = paste0("./blanks_",
                                      format(Sys.Date(), "%Y_%m_%d"), # like "2023_03_28"
                                      ".RData"))
    message(paste0("\nDone. The data has been exported as an RData file in the working directory."))
  } else {
    message("\nfileMethod is not \"save\" so no RData file will be saved.\n")
  }


  if (fileMethod == "return" || fileMethod == "both") {
    return(blanksPackage)
  } else {
    return(invisible())
  }

} # end extract_blanks().

#' Calculate mean ODs for blank data.
#'
#' This function takes a dataframe/tibble of blank data and returns a tibble of mean ODs grouped
#' by the argument `meanGroups`.
#'
#' @importFrom dplyr group_by across all_of summarize
#'
#' @param data A dataframe/tibble of blanks to calculate the mean ODs of.
#' @param meanGroups A character vector of variable names to group the means by. Usually passed
#' from `extract_blanks()` through the argument `meansBy`.
#'
#' @examples
#' get_mean_blanks(data, meanGroups)
#'
#' @return A tibble of meanODs with columns defined by `meanGroups`.
#'
#' @export

get_mean_blanks <- function(data, meanGroups){
  meanBlanks <- data |>
    # took me 2 whole days to figure out how to pass strings to group_by():
    dplyr::group_by(across(all_of(meanGroups))) |>
    dplyr::summarize(blankOD = mean(OD))

  return(meanBlanks)

} # end get_mean_blanks().
