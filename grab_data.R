params <-
list(user = "kareynol", file = "outputs/prob_site_adj_wgt_kar.csv")

## -----------------------------------------------------------------------------------------------------
#checkpoint::checkpoint("2022-11-17")
# # install the most recent approved version from CRAN
# install.packages("spsurvey")
# # load the most recent approved version from CRAN
library(spsurvey)

# view the citation
# citation(package = "spsurvey")
library(dplyr)
# source(here::here("harvesting_COMID_wgt_11_9_22.R"))
# source(here::here("Scripts/comid_wgt_join.R")) ##run these to match the COMID and wgts from the original EPA draw files. 
#As of 11/21/22 KAR identified that wgts have to be re-adjustd after determining what was actually sampled. This was updated and included in the analysis now.


# test the branch and commit


## ----read-site-list-----------------------------------------------------------------------------------
sites_list <- read.csv(here::here(params$file), stringsAsFactors = FALSE)


## ----get-site-info------------------------------------------------------------------------------------
# read in sites and initialize sites list to include all sites
# read in data that have the "master" tag
db_path <- paste("C:/Users/", params$user, "/New York State Office of Information Technology Services/SMAS - Streams Data Modernization", sep = "")
#
# sites_path <- file.path(
#   db_path,
#   "Cleaned Files",
#   "Final_Sites_ITS"
# )
# # Get the file paths for the filenames with the prefix "MASTER" and
# # extension CSV.
# sites_csv_list <- list.files(
#   path = sites_path,
#   pattern = "Master(.+?)csv",
#   full.names = TRUE
# )
# # Identify the appropriate name for each file path.
# sites_csv_names <- dplyr::case_when(
#   grepl("Master_S_Site", sites_csv_list) ~ "sites",
#   TRUE ~ "ERROR"
# )
# # Assign easy to reference names to filepaths.
# names(sites_csv_list) <- sites_csv_names
# # Reading in macro data -------------------------------------------------
# ## Loop through CSV list, import data, store in a list.
# sites_raw_list <- lapply(sites_csv_list, function(file_i) {
#   # Import data
#   read.csv(
#     file_i,
#     na.strings = c("", "NA"),
#     stringsAsFactors = FALSE,
#     fileEncoding = "UTF-8-BOM"
#   )})

# subset the site table
site.ex.l <- unique(sites_list$SMAS_ID)
# read in the sites file

# sites.short<-sites_raw_list$sites %>%
#  subset(SITE_HISTORY_ID %in% site.ex.l) #subset based on the list
# so we don't actually need this, we have basin andlat/long in the final file


## ----get-chemistry-and-pcode-data---------------------------------------------------------------------
# chem_path <- file.path(
#   db_path,
#   "Cleaned Files",
#   "Final_Chemistry_ITS"
# )
# # Get the file paths for the filenames with the prefix "MASTER" and
# # extension CSV.
# chem_csv_list <- list.files(
#   path = chem_path,
#   pattern = "MASTER(.+?)csv",
#   full.names = TRUE
# )
# # Identify the appropriate name for each file path.
# chem_csv_names <- case_when(
#   grepl("RESULT", chem_csv_list) ~ "result",
#   grepl("SAMPLE", chem_csv_list) ~ "sample",
#   grepl("PARAMETER", chem_csv_list) ~ "pcode",
#
#   TRUE ~ "ERROR"
# )
# # Assign easy to reference names to filepaths.
# names(chem_csv_list) <- chem_csv_names
# # Reading in macro data -------------------------------------------------
# ## Loop through CSV list, import data, store in a list.
# chem_raw_list <- lapply(chem_csv_list, function(file_i) {
#   # Import data
#   read.csv(
#     file_i,
#     na.strings = c("", "NA"),
#     stringsAsFactors = FALSE
#   )})
# # Join chem Data ----------------------------------------------------------
#
# chem.all<-merge(chem_raw_list$result,chem_raw_list$sample,
#                 by.x=c("CHR_SYS_SAMPLE_CDE","CHR_SAMPLE_DEL_GRP"),
#                 by.y=c("CHS_SYS_SAMPLE_CDE","CHS_SAMPLE_DEL_GRP"))
#
# chem.all<-chem.all %>%
#   subset(gdata::startsWith(CHS_DEC_SAMPLE_TYPE_CDE, "N")) %>%
#   subset(CHS_SAMPLE_SOURCE=="Field") %>%
#   subset(CHR_RESULT_TYPE_CDE %in% "TRG")
#
# #change both to numeric
# chem_raw_list$pcode$pcode.num<-as.numeric(chem_raw_list$pcode$CHEM_PARAMETER_PCODE)
#
#
# #merge pcode and chemistry
# chem<-merge(chem.all,chem_raw_list$pcode,by.x="CHR_PCODE",by.y="pcode.num",all.x = TRUE) %>%
#   #filter out lab pH, lab temperature, and lab specific conductance
#   filter(!(CHR_PCODE %in% c(110, 136, 139, 143, 145)))
#
# #clean up\
# rm(chem.all)


## ----get-event-table----------------------------------------------------------------------------------
field_path <- file.path(
  db_path,
  "Cleaned Files",
  "Final_SBU_Field_ITS"
)
# Get the file paths for the filenames with the prefix "MASTER" and
# extension CSV.
field_csv_list <- list.files(
  path = field_path,
  pattern = "(.+?)csv",
  full.names = TRUE
)
# Identify the appropriate name for each file path.
field_csv_names <- case_when(
  grepl("User_Perception", field_csv_list) ~ "userp",
  grepl("Habitat", field_csv_list) ~ "habitat",
  grepl("IN_SITU", field_csv_list) ~ "insitu",
  grepl("Sample_Event", field_csv_list) ~ "sample_info",
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

# merge insitu and pcode
field_raw_list$insitu$pcode.num <- as.numeric(field_raw_list$insitu$ISWC_CHEM_PARAMETER_PCODE_VALID)

# #merge pcode and insitu
# field_raw_list$insitu<-merge(field_raw_list$insitu,chem_raw_list$pcode,by="pcode.num",all.x = TRUE)


## ----get-macro-data-----------------------------------------------------------------------------------
# read in data that have the "master" tag
db_path <- paste("C:/Users/", params$user, "/New York State Office of Information Technology Services/SMAS - Streams Data Modernization", sep = "")

macro_path <- file.path(
  db_path,
  "Cleaned Files",
  "Final_Macro_ITS"
)
# Get the file paths for the filenames with the prefix "MASTER" and
# extension CSV.
macro_csv_list <- list.files(
  path = macro_path,
  pattern = "MASTER(.+?)csv",
  full.names = TRUE
)
# Identify the appropriate name for each file path.
macro_csv_names <- case_when(
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
  read.csv(
    file_i,
    na.strings = c("", "NA"),
    stringsAsFactors = FALSE
  )
})

# Join Macro Data ----------------------------------------------------------
metrics <- merge(
  x = macro_raw_list$metrics,
  y = macro_raw_list$bug_method,
  by.x = "MMDH_LINKED_ID_VALIDATOR",
  by.y = "MSSIH_LINKED_ID_VALIDATOR"
)

bugs_raw <- merge(
  x = macro_raw_list$raw_bugs,
  y = macro_raw_list$bug_method,
  by.x = "MSDH_LINKED_ID_VALIDATOR",
  by.y = "MSSIH_LINKED_ID_VALIDATOR"
)


## ----change-date-formats------------------------------------------------------------------------------
# change date on the insitu data and bugs raw
field_raw_list$insitu$ISWC_EVENT_SMAS_SAMPLE_DATE <- as.Date(field_raw_list$insitu$ISWC_EVENT_SMAS_SAMPLE_DATE, "%m/%d/%Y")
metrics$MSSIH_EVENT_SMAS_SAMPLE_DATE <- as.Date(metrics$MSSIH_EVENT_SMAS_SAMPLE_DATE, "%m/%d/%Y")
# chem$CHS_EVENT_SMAS_SAMPLE_DATE<-as.Date(chem$CHS_EVENT_SMAS_SAMPLE_DATE,"%m/%d/%Y")
# chem$CHS_SAMPLE_DATETIME<-as.POSIXct(chem$CHS_SAMPLE_DATETIME, format="%m/%d/%Y %H:%M")
field_raw_list$sample_info$SEIH_EVENT_SMAS_SAMPLE_DATE <- as.Date(field_raw_list$sample_info$SEIH_EVENT_SMAS_SAMPLE_DATE, "%m/%d/%Y")
field_raw_list$userp$UPFDH_EVENT_SMAS_SAMPLE_DATE <- as.Date(field_raw_list$userp$UPFDH_EVENT_SMAS_SAMPLE_DATE, "%m/%d/%Y")
# macro_raw_list$bug_method$MSSIH_EVENT_SMAS_SAMPLE_DATE<-as.Date(macro_raw_list$bug_method$MSSIH_EVENT_SMAS_SAMPLE_DATE,"%m/%d/%Y")
field_raw_list$habitat$HFDH_EVENT_SMAS_SAMPLE_DATE <- as.Date(field_raw_list$habitat$HFDH_EVENT_SMAS_SAMPLE_DATE, "%m/%d/%Y")
library(lubridate)
field_raw_list$insitu$year <- as.character(year(field_raw_list$insitu$ISWC_EVENT_SMAS_SAMPLE_DATE))


## ----subset-data-to-sites-list------------------------------------------------------------------------
sites.l <- unique(sites_list$SMAS_ID)

# get the sites first and then merge with the raw bugs and metrics by the validator.
macro.method <- macro_raw_list$bug_method %>%
  subset(MSSIH_EVENT_SMAS_HISTORY_ID %in% sites.l)
# make date align?

# now get the linked id validator for the bug stuff
bug.sites <- unique(macro.method$MSSIH_LINKED_ID_VALIDATOR)

metrics.df <- metrics %>%
  subset(MSSIH_EVENT_SMAS_HISTORY_ID %in% sites.l)

bugr.df <- bugs_raw %>%
  subset(MSDH_LINKED_ID_VALIDATOR %in% bug.sites)

# chem.df<-chem %>%
#   subset(CHS_EVENT_SMAS_HISTORY_ID %in% sites.l) %>%
#   filter(CHR_VALIDATOR_QUAL!="R")

habitat.df <- field_raw_list$habitat %>%
  subset(HFDH_EVENT_SMAS_HISTORY_ID %in% sites.l)

insitu.df <- field_raw_list$insitu %>%
  subset(ISWC_EVENT_SMAS_HISTORY_ID %in% sites.l)

userp.df <- field_raw_list$userp %>%
  subset(UPFDH_EVENT_SMAS_HISTORY_ID %in% sites.l)

sample_info.df <- field_raw_list$sample_info %>%
  subset(SEIH_EVENT_SMAS_HISTORY_ID %in% sites.l)


## -----------------------------------------------------------------------------------------------------

sites_list$filter_match <- paste(sites_list$SMAS_ID,
  sites_list$YEAR,
  sep = "_"
)


# create_match_column<-function(df,date_col,site_col){
#   df<-df
#   df$YEAR<-format(df$date_col,"%Y")
#   df$filter_match<-paste(df$date_col,df$site_col,sep = "_")
#   return(df)
# } this is not working :(

metrics.final <- metrics.df %>%
  mutate(
    YEAR = format(MSSIH_EVENT_SMAS_SAMPLE_DATE, "%Y"),
    filter_match = paste(MSSIH_EVENT_SMAS_HISTORY_ID,
      YEAR,
      sep = "_"
    )
  )
# join the metrics to the final sites file with the wgt

metrics_joined <- merge(sites_list,
  metrics.final,
  by = "filter_match"
)
unmatched <- anti_join(sites_list,
  metrics.final,
  by = "filter_match"
)
# join with sample info to see why they didn't match
# create the match id first
field_raw_list$sample_info <- field_raw_list$sample_info %>% mutate(
  YEAR = format(SEIH_EVENT_SMAS_SAMPLE_DATE, "%Y"),
  filter_match = paste(SEIH_EVENT_SMAS_HISTORY_ID,
    YEAR,
    sep = "_"
  )
)

unmatched_sample_info <- merge(unmatched, field_raw_list$sample_info,
  by = "filter_match"
)
# unmatched_sample_info<-unmatched_sample_info %>%
#   select(SMAS_ID,YEAR.x,COMID,SEIH_EVENT_SMAS_SAMPLE_DATE,SEIH_SITE_TYPE_PROBABILISTC,
#          SEIH_BIOSAMPLE_COLLECT,SEIH_BIOSAMPLE_TYPE) %>%
#   filter(SEIH_BIOSAMPLE_COLLECT==TRUE)
# double checked the 2021-these are low gradient samples; not sure about the rest of them


## ----run-spsurvey package-----------------------------------------------------------------------------
# open the template data
# load(file='data/NE_Lakes.rda')

metrics_joined$YEAR <- metrics_joined$YEAR.x
metrics_joined <- metrics_joined %>%
  dplyr::rename(latitude = LAT, longitude = LONG)
# get basin in there
metrics_joined$BASIN <- as.numeric(substr(metrics_joined$SMAS_ID, start = 1, stop = 2))


metrics_joined_last_cycle <- metrics_joined %>%
  filter(YEAR >= 2017 & YEAR <= 2021)



metrics.sf <- sf::st_as_sf(metrics_joined_last_cycle,
  coords = c("longitude", "latitude"),
  crs = "+proj=longlat +datum=WGS84 +no_defs"
)



cont_ests <- spsurvey::cont_analysis(metrics.sf,
  siteID = "MSSIH_EVENT_SMAS_HISTORY_ID",
  vars = "MMDH_BIO_ASMT_PROFILE_SCORE",
  subpops = "BASIN",
  weight = "adj_wgt" #using the adjusted weights calculatd on 11/22/22 KAR
)
# statewide
# cont_ests<-spsurvey::cont_analysis(metrics.sf,
#                                    siteID="MSSIH_EVENT_SMAS_HISTORY_ID",
#                                    vars="MMDH_BIO_ASMT_PROFILE_SCORE",
#                                    #subpops = "SITE_BASIN",
#                                    weight="wgt")

spsurvey::cdf_plot(cont_ests$CDF)

# save the results to outputs
write.csv(cont_ests$Mean, "outputs/mean_subpop_basin_17_21_cycle.csv")

spsurvey::sp_summary(metrics.sf, formula = MMDH_BIO_ASMT_PROFILE_SCORE ~ BASIN)
spsurvey::sp_plot(metrics.sf, formula = MMDH_BIO_ASMT_PROFILE_SCORE ~ BASIN)


## ----trend-data---------------------------------------------------------------------------------------
#as of 11/22/22 this is not working, but will be fun to try

# metrics_all.sf <- sf::st_as_sf(metrics_joined,
#   coords = c("longitude", "latitude"),
#   crs = "+proj=longlat +datum=WGS84 +no_defs"
# )
# metrics_all.sf$YEAR <- as.factor(metrics_all.sf$YEAR)
# 
# change <- spsurvey::change_analysis(
#   metrics_all.sf,
#   siteID = "MSSIH_EVENT_SMAS_HISTORY_ID",
#   vars_cont = "MMDH_BIO_ASMT_PROFILE_SCORE",
#   surveyID = "YEAR",
#   weight = "wgt"
# )
# 
# 
# ex_run <- spsurvey::change_analysis(
#   ex,
#   siteID = "SITE_ID",
#   vars_cont = "BMMI",
#   vars_cat = "NITR_COND",
#   surveyID = "YEAR",
#   weight = "WEIGHT"
# )
# 
# ex <- spsurvey::NRSA_EPA7

