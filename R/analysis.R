#' Analyze grow96-compatible datasets.
#'
#' `quick_analyze()` automatically analyzes any number of individual `grow96`-compatible datasets
#' and outputs processed and/or summarized growth data as dataframes in a list.
#'
#' @details
#' Each project's data files must be contained in a separate folder, and all project folders
#' placed in a single "hub" folder, the path for which is the argument `dataHubPath`.
#' By default, analyzed data is exported to the hub folder, as an RData object with filename
#' `growthData_YYYY_MM_DD.RData`. The exported data is a list with as many elements as projects,
#' each element named according to project folder names. Only summarized data ("analyzed_data")
#' is exported by default, but unsummarized data ("processed_data") can also be exported by
#' setting `exportAll` to `TRUE`.
#' @details
#' OD reads are blanked by calling `auto_blank()`. If necessary, blank values can be supplied
#' through the argument `blankData`, which is then used to blank OD data if a project does not
#' have valid blank data available. `forceBlanking` can be set to TRUE to force the use of
#' `blankData` regardless of whether blanks are available in a project.
#' ## Renaming column names
#' This function can rename slightly inconsistent variable names (such as `Drug` vs. `drugs`).
#' When `testNames` is a character vector of correct column names, it is passed to the helper
#' function [`auto_rename()`] and is used to check and fix incorrect column names.
#'
#' @param dataHubPath The path of the "hub" folder containing project folders to analyze.
#' @param forceBlanking If `TRUE`, `blankData` is used for blanking regardless of whether
#' blank data exists in a particular project. Defaults to `FALSE`.
#' @param blankData A dataframe/tibble containing blank values. See [grow96::blankODs]
#' for more information.
#' @param testNames A character vector of column names to check and fix if necessary. See Details
#' for more information.
#' @param exportAll Defaults to `FALSE`. If `TRUE`, unsummarized data will also be exported.
#' @param exportOutput Determines whether the output is saved as an RData file. Defaults to `TRUE`.
#' @param returnOutput Determines whether the function returns the output. Defaults to `FALSE`.
#'
#' @examples
#' quick_analyze(dataHubPath,
#'               forceBlanking = FALSE,
#'               blankData = NULL,
#'               exportAll = FALSE,
#'               exportOutput = TRUE,
#'               returnOutput = FALSE)
#'
#' @return A list of *n* elements, corresponding to *n* projects, each containing a tibble of
#' processed and/or summarized OD data.
#'
#' @export

quick_analyze <- function(dataHubPath,
                          forceBlanking = FALSE,
                          blankData = NULL,
                          testNames = NULL,
                          exportAll = FALSE,
                          exportOutput = TRUE,
                          returnOutput = FALSE){

  # Catch conflicting flags
  if(!exportOutput & !returnOutput){
    stop("\nexportOutput and returnOutput can't both be FALSE, unless you want to receive nothing.")
  }

  # Parse file path
  folders <- dir(dataHubPath) # i.e., what projects are going to be analyzed?

  # # Load blanks
  # blanks <- readRDS(path)

  # Prepare final export object
  dataPackage <- list()
  # dataPackage <- vector(mode = "list", length = length(folders))
  # names(dataPackage) <- folders # explicitly name each element

  for(project in 1:length(folders)){
    message(paste("\nWorking on project", folders[project], "now..."))

    # Define project path
    projectPath <- paste0(dataHubPath, "/", project)

    # Determine the raw file prefix and process data accordingly
    processedData <- auto_process(projectPath)
    # Optional: rename columns
    if(!is.null(testNames)){
      message("Checking column names against supplied ones...")
      processedData <- auto_rename(processedData, testNames)
    }
    # Generate QC report and create value indicating if QC detected blank data
    hasBlanks <- auto_QC(processedData, projectPath)
    # Blank the data appropriately
    blankedData <- auto_blank(processedData, forceBlanking, hasBlanks, blankData)
    # Analyze the data
    analyzedData <- grow96::analyseODData(blankedData)

    # Add the data to the appropriate place in the final export list
    if(exportAll){
      dataPackage[[folders[project]]] <- list("processed_data" = blankedData,
                                              "analyzed_data" = analyzedData)
    } else {
      dataPackage[[folders[project]]] <- list("analyzed_data" = analyzedData)
      # do I actually need to use list() here?
    }

  }

  # Rename export object names, removing whitespaces
  names(dataPackage) <- chartr(" ", "_", names(dataPackage))

  # Optional: export data package as RData to root data folder
  if(exportOutput){
    save(dataPackage, file = paste0(dataHubPath, "/growthData_",
                                    format(Sys.Date(), "%Y_%m_%d"), # "2023_03_28"
                                    ".RData"))
    message(paste0("\nDone. The data has been exported as an RData file in ",
                   dataHubPath, "."))
  } else {
    message("\n\"exportOutput\" is FALSE so there will be no RData file saved!\n")
  }

  if (returnOutput) {
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
#' When `testNames` is a character vector of correct column names, it is passed to the helper
#' function [`auto_rename()`] and is used to check and fix incorrect column names.
#'
#' @importFrom dplyr bind_rows
#'
#' @inheritParams quick_analyze
#' @param meansBy A character vector of variables to group means by. If this argument is `NULL`
#' (the default), no mean blank ODs will be generated. See Details for more information.
#' @param combineProjects Defaults to `FALSE`. If `TRUE`, an additional dataframe called
#' "all_blanks" will be generated by combining dataframes from all projects. This argument will
#' automatically be `TRUE` if `meansBy` is defined.
#'
#' @examples
#' extract_blanks(dataHubPath,
#'                testNames = NULL,
#'                meansBy = NULL,
#'                combineProjects = FALSE,
#'                exportOutput = TRUE,
#'                returnOutput = FALSE)
#'
#' @return A list of tibbles of blank OD data. May include additional tibbles for combined blanks
#' and mean blank ODs depending on function arguments.
#'
#' @export

extract_blanks <- function(dataHubPath,
                           testNames = NULL,
                           meansBy = NULL,
                           combineProjects = FALSE,
                           exportOutput = TRUE,
                           returnOutput = FALSE){

  # Catch conflicting flags
  if(!exportOutput & !returnOutput){
    stop("\nexportOutput and returnOutput can't both be FALSE, unless you want to receive nothing.")
  }

  # Parse file path
  folders <- dir(dataHubPath) # i.e., what projects are going to be analyzed?

  # Prepare final export object
  blanksPackage <- list()

  for(project in 1:length(folders)){
    message(paste("\nWorking on project", folders[project], "now..."))

    # Define project path
    projectPath <- paste0(dataHubPath, "/", folders[project])

    # Determine the raw file prefix and process data accordingly
    processedData <- auto_process(projectPath)

    # Optional: rename columns
    if(!is.null(testNames)){
      message("Checking column names against supplied ones...")
      processedData <- auto_rename(processedData, testNames)
    }

    # Extract all rows with BLANK in column <WellType>, and add to the final export list
    blanksOnly <- processedData[which(processedData$WellType == "BLANK"), ]
    blanksPackage[[folders[project]]] <- blanksOnly
    if(nrow(blanksOnly) == 0){ # warn if a project exports no rows.
      warning(paste0(folders[project]," appears to have no valid BLANK values."))
    }

  }

  # Force combineProjects if meansBy has values indicating means are needed
  if(!is.null(meansBy) & !combineProjects){
    message("Mean ODs are requested but combineProjects = FALSE; forcing combineProjects = TRUE.")
    combineProjects <- TRUE
  }

  # Optional: glue individual project outputs into a single dataframe "all_blanks"
  if(combineProjects){
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
  if(exportOutput){
    save(blanksPackage, file = paste0(dataHubPath, "/blanks_",
                                      format(Sys.Date(), "%Y %m %d"), # like "2023 03 28"
                                      ".RData"))
    message(paste0("\nDone. The blanks have been exported as an RData file in ",
                   dataHubPath, "."))
  } else {
    message("\n\"exportOutput\" is FALSE so there will be no RData file saved!\n")
  }


  if (returnOutput) {
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
