

#' grab_sites
#'
#' @param path this is the specified path to your data
#' @param full this is an argument to return the full dataset (TRUE) or as shortened dataset
#' with just the most relevant dataframes.
#'
#' @return 1 data frame for sites table (sites), and 3 lists for other data;
#' Chem_raw_list for chemistry parameters, field_raw_list for habitat, insitu, sample_info and
#' user perception; and macro_raw_list for metrics, method and raw bugs.
#' @export
#'
#' @examples grab_data() will grab from the L drive

grab_sites <- function(path = "L:/DOW/BWAM Share/SMAS/data/cleaned_files",
                      full=TRUE) {
  
  db_path <- file.path(path)
  
  sites_path <- file.path(db_path,
                          "Final_Sites_ITS")
  
  cat("(1/2) Grabbing master sites file.  \n")
  # Get the file paths for the filenames with the prefix "MASTER" and
  # extension CSV.
  sites_csv_list <- list.files(path = sites_path,
                               pattern = "Master(.+?)csv",
                               full.names = TRUE)
  # Identify the appropriate name for each file path.
  sites_csv_names <- ifelse(
    test = grepl("Master_S_Site", sites_csv_list),
    yes = "sites",
    no = "ERROR"
  )
  # Assign easy to reference names to filepaths.
  names(sites_csv_list) <- sites_csv_names
  # Reading in sites data -------------------------------------------------
  ## Loop through CSV list, import data, store in a list.
  sites_raw_list <- lapply(sites_csv_list, function(file_i) {
    # Import data
    read.csv(
      file_i,
      na.strings = c("", "NA"),
      stringsAsFactors = FALSE,
      fileEncoding = "UTF-8-BOM"
    )
  })
  sites <- sites_raw_list$sites
  rm(sites_raw_list)
  
 
  if(full){

    .GlobalEnv$sites <- sites}
  
  if(!full){

    .GlobalEnv$sites <- sites
  }
  
  cat("(2/2) DONE! \n")
}

