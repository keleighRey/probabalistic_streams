

#' grab_field
#'
#' @param path this is the specified path to your data
#' @param full this is an argument to return the full dataset (TRUE) or as shortened dataset
#' with just the most relevant dataframes.
#'
#' @return field_raw_list for habitat, insitu, sample_info and
#' user perception (full = TRUE), or a set of 3 dataframes for the above (full = FALSE)
#' @export
#'
#' @examples grab_field() will grab from the L drive

grab_field <- function(path = "L:/DOW/BWAM Share/SMAS/data/cleaned_files",
                      full=TRUE) {
  
  db_path <- file.path(path)
  
  ## ----get-event-table----------------------------------------------------------------------------------
  field_path <- file.path(db_path,
                          "Final_SBU_Field_ITS")
  cat("(1/3) Grabbing field files. \n")
  
  # Get the file paths for the filenames with the prefix "MASTER" and
  # extension CSV.
  field_csv_list <- list.files(path = field_path,
                               pattern = "(.+?)csv",
                               full.names = TRUE)
  # Identify the appropriate name for each file path.
  field_csv_names <- dplyr::case_when(
    grepl("User_Perception", field_csv_list) ~ "userp",
    grepl("Habitat", field_csv_list) ~ "habitat",
    grepl("IN_SITU", field_csv_list) ~ "insitu",
    grepl("Sample_Event", field_csv_list) ~ "sample_info",
    grepl("Macro_field", field_csv_list) ~ "macro_field",
    TRUE ~ "ERROR"
  )
  # Assign easy to reference names to filepaths.
  names(field_csv_list) <- field_csv_names
  # Reading in macro data -------------------------------------------------
  ## Loop through CSV list, import data, store in a list.
  field_raw_list <- lapply(field_csv_list, function(file_i) {
    # Import data
    read.csv(
      file_i,
      na.strings = c("", "NA"),
      stringsAsFactors = FALSE,
      fileEncoding = "UTF-8-BOM"
    )
  })
  cat("(2/3) Grabbing pcode file and merging. \n")
  chem_path <- file.path(db_path,
                         "Final_Chemistry_ITS")
  chem_csv_list <- list.files(path = chem_path,
                              pattern = "MASTER(.+?)csv",
                              full.names = TRUE)
  # Identify the appropriate name for each file path.
  chem_csv_names <- dplyr::case_when(
    grepl("RESULT", chem_csv_list) ~ "result",
    grepl("SAMPLE", chem_csv_list) ~ "sample",
    grepl("PARAMETER", chem_csv_list) ~ "pcode",
    grepl("QUALIFIER_LAB", chem_csv_list) ~ "lab_qualifiers",
    grepl("VALIDATOR", chem_csv_list) ~ "qualifier_validator",
    TRUE ~ "ERROR"
  )
  # Assign easy to reference names to filepaths.
  names(chem_csv_list) <- chem_csv_names
  # Reading in macro data -------------------------------------------------
  # Loop through CSV list, import data, store in a list.
  chem_raw_list <- lapply(chem_csv_list, function(file_i) {
    # Import data
    read.csv(file_i,
             na.strings = c("", "NA"),
             stringsAsFactors = FALSE)
  })
  #change both to numeric
  chem_raw_list$pcode$pcode.num <-
    as.numeric(chem_raw_list$pcode$CHEM_PARAMETER_PCODE)
  
  # merge insitu and pcode
  field_raw_list$insitu$pcode.num <-
    as.numeric(field_raw_list$insitu$ISWC_CHEM_PARAMETER_PCODE_VALID)
  field_raw_list$insitu <-
    merge(field_raw_list$insitu,
          chem_raw_list$pcode,
          by = "pcode.num",
          all.x = TRUE)
 rm(chem_raw_list) 
 
  cat("(3/3) Updating date columns for standard formats. \n")
  ## ----change-date-formats------------------------------------------------------------------------------
  # change date on the final list objects to use
  field_raw_list$insitu$ISWC_EVENT_SMAS_SAMPLE_DATE <-
    as.Date(field_raw_list$insitu$ISWC_EVENT_SMAS_SAMPLE_DATE,
            "%m/%d/%Y")
 
  field_raw_list$sample_info$SEIH_EVENT_SMAS_SAMPLE_DATE <-
    as.Date(field_raw_list$sample_info$SEIH_EVENT_SMAS_SAMPLE_DATE,
            "%m/%d/%Y")
  field_raw_list$userp$UPFDH_EVENT_SMAS_SAMPLE_DATE <-
    as.Date(field_raw_list$userp$UPFDH_EVENT_SMAS_SAMPLE_DATE,
            "%m/%d/%Y")

  field_raw_list$habitat$HFDH_EVENT_SMAS_SAMPLE_DATE <-
    as.Date(field_raw_list$habitat$HFDH_EVENT_SMAS_SAMPLE_DATE,
            "%m/%d/%Y")
  if(full){
    .GlobalEnv$field_raw_list <- field_raw_list

}
  
  if(!full){
    .GlobalEnv$insitu <- field_raw_list$insitu
    .GlobalEnv$habitat <- field_raw_list$habitat
    .GlobalEnv$userp <- field_raw_list$userp

  }
  
  cat("DONE! \n")
}

