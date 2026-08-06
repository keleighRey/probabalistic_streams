#' grab_chem
#'
#' @param path this is the specified path to your data
#' @param full this is an argument to return the full dataset (TRUE) or as shortened dataset
#' with just the most relevant data frames.
#'
#' @return list of all chem data (full = TRUE), or a dataframe of joined chemistry data (full = FALSE)
#' @export
#'
#' @examples grab_data() will grab from the L drive
#' 
grab_chem <- function(path = "L:/DOW/BWAM Share/SMAS/data/cleaned_files",
                      full=TRUE) {
  
  db_path <- file.path(path)
  
  ## ----get-chemistry-and-pcode-data---------------------------------------------------------------------
  chem_path <- file.path(db_path,
                         "Final_Chemistry_ITS")
  cat("(1/2) Grabbing chemistry files. \n")
  # Get the file paths for the filenames with the prefix "MASTER" and
  # extension CSV.
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
  # Join chem Data ----------------------------------------------------------
  
  chem_raw_list$chem_all <-
    merge(
      chem_raw_list$result,
      chem_raw_list$sample,
      by.x = c("CHR_SYS_SAMPLE_CDE", "CHR_SAMPLE_DEL_GRP"),
      by.y = c("CHS_SYS_SAMPLE_CDE", "CHS_SAMPLE_DEL_GRP")
    )
  
  chem_raw_list$chem_all <- chem_raw_list$chem_all %>%
    subset(startsWith(CHS_DEC_SAMPLE_TYPE_CDE, "N")) %>%
    subset(CHS_SAMPLE_SOURCE == "Field") %>%
    subset(CHR_RESULT_TYPE_CDE %in% "TRG")
  
  #change both to numeric
  chem_raw_list$pcode$pcode.num <-
    as.numeric(chem_raw_list$pcode$CHEM_PARAMETER_PCODE)
  
  #merge pcode and chemistry
  chem_raw_list$chem_final <-
    merge(
      chem_raw_list$chem_all,
      chem_raw_list$pcode,
      by.x = "CHR_PCODE",
      by.y = "pcode.num",
      all.x = TRUE
    ) %>%
    #filter out lab pH, lab temperature, and lab specific conductance
    dplyr::filter(!(CHR_PCODE %in% c(110, 136, 139, 143, 145))) %>%
    dplyr::filter(CHR_VALIDATOR_QUAL != "R")
  
 
  cat("(2/2) Updating date columns for standard formats. \n")
  ## ----change-date-formats------------------------------------------------------------------------------
  # change date on the final list objects to use

  chem_raw_list$chem_final$CHS_EVENT_SMAS_SAMPLE_DATE <-
    as.Date(chem_raw_list$chem_final$CHS_EVENT_SMAS_SAMPLE_DATE,
            "%m/%d/%Y")

  if(full){
    .GlobalEnv$chem_raw_list <- chem_raw_list
  }
  
  if(!full){

    .GlobalEnv$chem <- chem_raw_list$chem_final
 
  }
  
  cat("DONE! \n")
}

