#Keleigh Reynolds
#11/9/2022
#grab EPA files for COMID and weight

library("dplyr")												
library("plyr")												
library("readr")
library("readxl")

# prob_data <- list.files(path = "C:/Users/kareynol/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Random_Probabalistic_Assignment/Possible Relevant Historical Data/epa_draw_all",	
#                        pattern = "*.xlsx",
#                        full.names = TRUE) #this isn;t working, but itcould

prob_1<-readxl::read_excel(path=here::here("data/NY_Basin_Design_2018_SummaryNotesFromSiteSelection.xlsx"),
                           sheet = 1)
prob_2<-readxl::read_excel(path=here::here("data/NY_Basin_Design_2013_SummaryNotesFromSiteSelection.xlsx"),
                           sheet = 1)
prob_3<-readxl::read_excel(path=here::here("data/NY_Basin_2008_gis_export.xlsx"),
                           sheet = 1)

#create a use vector for if it was used or not
used_site<-function(df){   
  df<-df %>% 
    mutate(use=case_when(status=="TS"~TRUE,
                         TRUE~FALSE))
}

#create the true/false column needed to adjust the weights
prob_1<-used_site(prob_1)
prob_2<-used_site(prob_2)
prob_3<-used_site(prob_3)


#now we have to adjust the weights by each draw frame
# i used the identified sites to confirm the TS for the base samples, any that were base and
#not sampled got a PB or physical barrier. The overdraws that were not sampled got "NN" or not
#needed, "NT" was reserved for those that were tidally influenced
#The status column was added by hand looking at the confirmed sites and the notes in the original
#file

adjust_weight<-function(df){
  
  wgt<-df$wgt
  wgtcat<-df$mdcaty
  sites<-df$use
  
  framesize_x<-df %>% 
    group_by(mdcaty) %>% 
    dplyr::summarise(n=sum(wgt))
  
  x1<-framesize_x$mdcaty
  x2<-framesize_x$n
  framesize<-setNames(x2,x1)
  
  x<-spsurvey::adjwgt(wgt,wgtcat,framesize,sites)
  
  df_adj<-cbind(df,x) #cbind might not be ok, mismatches might be happening
  #try mutate instead
  return(df_adj)
}

prob_1_adjusted<-adjust_weight(prob_1)
prob_2_adjusted<-adjust_weight(prob_2)
prob_3_adjusted<-adjust_weight(prob_3)


#select columns from the dataframes
prob_1_short<-prob_1_adjusted %>% 
  dplyr::select(COMID,wgt,x) %>% 
  dplyr::rename(adj_wgt=x) %>% 
  mutate(draw_year="2018_2022")

prob_2_short<-prob_2_adjusted %>% 
  dplyr::select(COMID,wgt,x) %>% 
  dplyr::rename(adj_wgt=x) %>% 
  mutate(draw_year="2013_2018")

prob_3_short<-prob_3_adjusted %>% 
  dplyr::select(COMID,wgt,x) %>% 
  dplyr::rename(adj_wgt=x) %>%  
  mutate(draw_year="2008_2013")


#bind them together
prob_all<-rbind(prob_1_short,prob_2_short,prob_3_short)

prob_all<-prob_all %>% 
  distinct() #take out any that are duplicated-although there are, we may not want
#to do the draw year?
#look at the duplicates
 
dups <-prob_all[duplicated(prob_all$COMID)|duplicated(prob_all$COMID, fromLast=TRUE),]
dups
#so i thinnk we'll have to bind by draw year AND COMID, since they seem duplicated. 


write.csv(prob_all,"outputs/comid_adj_wgt_all_years.csv")

