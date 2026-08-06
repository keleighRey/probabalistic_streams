

#' grab_bugs
#'
#' @param path this is the specified path to your data
#' @param full this is an argument to return the full dataset (TRUE) or as shortened dataset
#' with just the most relevant dataframes.
#'
#' @return 1 data frame for metrics file (full = FALSE) or macro_raw_list for metrics, method and raw bugs (full = TRUE).
#' @export
#'
#' @examples grab_bugs() will grab from the L drive

grab_bugs <- function(path = "L:/DOW/BWAM Share/SMAS/data/cleaned_files",
                      full=TRUE) {
  
  db_path <- file.path(path)

  
  ## ----get-macro-data-----------------------------------------------------------------------------------
  # read in data that have the "master" tag
  cat("(1/2) Grabbing bug files.  \n")
  macro_path <- file.path(db_path,
                          "Final_Macro_ITS")
  # Get the file paths for the filenames with the prefix "MASTER" and
  # extension CSV.
  macro_csv_list <- list.files(path = macro_path,
                               pattern = "MASTER(.+?)csv",
                               full.names = TRUE)
  # Identify the appropriate name for each file path.
  macro_csv_names <- dplyr::case_when(
    grepl("METRICS", macro_csv_list) ~ "metrics",
    grepl("SPECIES_SAMP_INF", macro_csv_list) ~ "bug_method",
    grepl("SPECIES_DATA_HISTORY", macro_csv_list) ~ "raw_bugs",
    TRUE ~ "ERROR"
  )
  # Assign easy to reference names to filepaths.
  names(macro_csv_list) <- macro_csv_names
  # Reading in macro data -------------------------------------------------
  ## Loop through CSV list, import data, store in a list.
  macro_raw_list <- lapply(macro_csv_list, function(file_i) {
    # Import data
    read.csv(file_i,
             na.strings = c("", "NA"),
             stringsAsFactors = FALSE)
  })
  
  # Join Macro Data ----------------------------------------------------------
  macro_raw_list$metrics_all <- merge(
    x = macro_raw_list$metrics,
    y = macro_raw_list$bug_method,
    by.x = "MMDH_LINKED_ID_VALIDATOR",
    by.y = "MSSIH_LINKED_ID_VALIDATOR"
  )
  
  macro_raw_list$bugs_raw_all <- merge(
    x = macro_raw_list$raw_bugs,
    y = macro_raw_list$bug_method,
    by.x = "MSDH_LINKED_ID_VALIDATOR",
    by.y = "MSSIH_LINKED_ID_VALIDATOR"
  )
  
  cat("(2/2) Updating date columns for standard formats. \n")
  ## ----change-date-formats------------------------------------------------------------------------------
  # change date on the final list objects to use
  macro_raw_list$metrics_all$MSSIH_EVENT_SMAS_SAMPLE_DATE <-
    as.Date(macro_raw_list$metrics_all$MSSIH_EVENT_SMAS_SAMPLE_DATE,
            "%m/%d/%Y")
  macro_raw_list$bug_method$MSSIH_EVENT_SMAS_SAMPLE_DATE <-
    as.Date(macro_raw_list$bug_method$MSSIH_EVENT_SMAS_SAMPLE_DATE,
            "%m/%d/%Y")

  if(full){

    .GlobalEnv$macro_raw_list <- macro_raw_list
}
  
  if(!full){

    .GlobalEnv$metrics <- macro_raw_list$metrics_all

  }
  
  cat("DONE! \n")
}

