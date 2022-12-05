#11/14/2022 Makenzie Garrett
#working on joining the cleaned site list and the wgt from the original EPA draw files

library(readr)
library(dplyr)
library(tidyr)

clean_v2 <- read_csv(
  file = here::here("data",
                    "Final_Cleaned_MG_11_14_22.csv") #this is the cleaned file from MG and EJ
  )
comid_wgt <- read_csv(
  file = here::here("outputs",
                    "comid_adj_wgt_all_years.csv")#this is the EPA file created by KR that has 
  #just COMID, wgt and draw year (which was not set up quite correctly.)
)
comid_wgt$...1 <- NULL #cleaning up the columns
comid <- separate(
  data = comid_wgt, 
  col = draw_year, 
  sep = "_",
  into = c("start", "end")#separating the draw years into start year and end year
)

joined <- left_join(
  x = clean_v2,
  y = comid,
  by = "COMID"#joined the df's by just comid, this created many duplicates
)

sub_df <- filter(
  .data = joined, 
  YEAR >= start & YEAR <= end #this filtered teh data set to be within the draw years
)

# anti_df <- anti_join(
#   x = sub_df,
#   y = clean_v2,
#   by = c("COMID",
#          "YEAR",
#          "SMAS_ID") #this was to see what didn't join, this is empty
# )

test <- clean_v2[!(clean_v2$COMID %in% sub_df$COMID), ] #this is looking for COMIDs that
#existed in the original file, but now don't exist in teh joined file that is between the draws

test$corrected_comid <- "" #creating a new column to correct COMID's
test_adj <- test %>% 
  mutate(corrected_comid = case_when(
    COMID == "3246098"~"3246096",
    TRUE~""
  ),
  NOTES = case_when(#and to make notes about what should be in there
    COMID == "22294818"~ifelse(!is.na(NOTES), paste(NOTES, "Not sampled as probabilistic."), "Not sampled as probabilistic."),
    COMID == "4151524"~ifelse(!is.na(NOTES), paste(NOTES, "Not sampled as probabilistic."), "Not sampled as probabilistic."),
    COMID == "15567569"~ifelse(!is.na(NOTES), paste(NOTES, "Not sampled as probabilistic."), "Not sampled as probabilistic."),
    COMID == "15445117"~ifelse(!is.na(NOTES), paste(NOTES, "Not sampled as probabilistic."), "Not sampled as probabilistic."),
    COMID == "8119017"~ifelse(!is.na(NOTES), paste(NOTES, "Predates draws."), "Predates draws."),
    COMID == "22751809"~ifelse(!is.na(NOTES), paste(NOTES, "Predates draws."), "Predates draws."),
    COMID == "22741107"~ifelse(!is.na(NOTES), paste(NOTES, "Predates draws."), "Predates draws."),
    TRUE~NOTES
  ))

test_adj <- test_adj %>% filter(
  !is.na(SMAS_ID) 
) #test_adj are the ones that are MISSNG from the sub_df dataframe, or they existed in the 
#clean file and not when we filtered to the draw dates. THe notes above say what we are doing with them.



dups <- sub_df %>%  #this is looking at the duplicate values in COMID and year
  group_by(COMID, YEAR) %>% 
  dplyr::mutate(n = n()) %>% 
  filter(n > 1)

dups_adj <- dups %>% 
  mutate(keep = case_when( #creating a column of those what we want to keep-i think we actually
    #have to filter out the ones that say false from the sub_df data frame, since these were filtered
    #from the sub_df file
    SMAS_ID == "06-GREA_W-1.1" & start == "2018"~TRUE,
    SMAS_ID == "10-MCKN-6.4" & start == "2013"~TRUE,
    SMAS_ID == "10-METT-17.7" & start == "2013"~TRUE,
    SMAS_ID == "10-RORE-2.3" & start == "2013"~TRUE,
    SMAS_ID == "17-BRNX-12.3" & start == "2013"~TRUE,
    SMAS_ID == "17-BRNX-9.2" & start == "2013"~TRUE,
    SMAS_ID == "17-MANH-1.2" & start == "2013"~TRUE,
    SMAS_ID == "17-MILR-3.5" & start == "2013"~TRUE,
    SMAS_ID == "17-PECN-3.2" & start == "2013"~TRUE,
    SMAS_ID == "13-NORM-7.5" & start == "2008"~TRUE,
    TRUE~FALSE
  ))# %>% 
 # filter(keep == TRUE) # i think we dont want to do this

#Removed and cleaned duplicates from clean_v2 to sub_df
#dups_adj has notes about what to do with records that exist in the sub_df file
#test_adj is a df that has missing records, these we will have to bind to the sub_df file,
#as of 11/15/22 we only need to keep 1 of these records, and adjust the COMID

test_adj2 <- test_adj %>% 
  mutate(COMID=case_when(
    COMID=="3246098"~paste(as.numeric("3246096")),
    TRUE~""
  ))#correcting the COMID

test_adj2$corrected_comid <- NULL #removing the corrected COMID column
test_adj2$COMID<-as.double(test_adj2$COMID) #getting them in the correct format

#so now we have to bind with the original EPA ID file (comid_wgt) to get the rest of teh columns

joined2 <- left_join(
  x = test_adj2,
  y = comid,
  by = "COMID"#joined the df's by just comid, this created many duplicates
)

joined3 <- filter(
  .data = joined2, 
  YEAR >= start & YEAR <= end #this filtered teh data set to be within the draw years
  #and now this one has just the one missing record we identified as missing from the test_df
  #data frame-FOX-1.1 with the corrected COMID.
)

sub_df_2<-rbind(sub_df,joined3) #append the missing record


#####dupicates!
#now we have the dups_adj field with the records we either want to keep or get rid of
#all of these exist in the sub_df dataframe right now, so we want to get rid of the ones 
#that are duplicated and shouldn't be in there. keep=FALSE

dups_adj_2<-dups_adj %>% 
  mutate(matching=paste(SMAS_ID,#creating match column to get rid of the ones we want to
                     COMID,
                     YEAR,
                     start,
                     sep="_")) %>% 
  filter(keep==FALSE) #create a dataframe for just those that we want to get rid of

remove.l<-unique(dups_adj_2$matching)#create a vector of just those matching id's

sub_df_2<-sub_df_2 %>% 
  mutate(matching=paste(SMAS_ID,
                     COMID,
                     YEAR,
                     start,
                     sep="_")) #creating the same match column to get rid of the ones we want to
  
#now we can filter out the ones that we identified as keep=FALSE in the dups_adj df by using 
#the match id and vector of the ones to get rid of

sub_df_3<-sub_df_2 %>% 
  filter(!matching %in% remove.l) #the ! says get filter the df to the ones that ARENT in the vector

#so that looks like it did it, it removed 10 records from the df and we had 10 records we marked
#to remove. so all of these matched by COMID, year and date start. 
# I beleive it IS possible we have duplicate COMIDs, but they will have different matching years
#so that's ok.

write.csv(sub_df_3,"outputs/prob_site_adj_wgt_kar.csv")
