# workspace
# an attempt to expand auto_rename() to allow multiple keys and renaming runs

if(!is.null(renameKey)){

  for(key in renameKey){

    # define the column to change
    targetCol <- names(key[1])
    message(paste0("\nFixing values in column \"", targetCol, "\":"))

    # extract a sub-key containing only reference and relevant variant column
    subKey <- key |>
      dplyr::select(all_of(targetCol), # reference is determined by first col of key
                    projectName)|>  # variant is relative to project
      # dplyr::filter(!is.na(folders[project])) # doesn't seem to work on colnames?
      # dplyr::filter(complete.cases(.)) # this doesn't work either, people be lyin on the internet
      na.omit()

    # construct vector for replacement
    replacements <- subKey[[targetCol]][match(data[[targetCol]], subKey[, 2])] # new values
    originals <- data[[targetCol]][!is.na(replacements)] # old values

    # preserve the old names as a new column
    data[[paste0(targetCol, "_old")]] <- data[[targetCol]]

    if(all(is.na(replacements))){
      # replacements are all NA, so all() returns TRUE
      message("No valid replacements in this column.\n")
    } else {
      # replace elements of data$`targetCol` that match non-NA replacements
      data[[targetCol]][!is.na(replacements)] <- replacements[!is.na(replacements)]
      # currently replaces even identical values; not sure if I want to restrict this to
      #  only cases where values in replacements don't match target.

      # report changes because of paranoia (maybe this should be a warning?)
      message(paste(unique(originals[!is.na(originals)]),
                    "replaced with", unique(replacements[!is.na(replacements)]), "\n"))
    } # known bug: if length(originals) != replacements, the list loops around and reports incorrectly.

  }


}

### Fix 2: values in a column ###
if(!is.null(renameKey)){

}
