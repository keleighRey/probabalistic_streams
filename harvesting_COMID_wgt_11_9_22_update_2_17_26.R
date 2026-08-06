#Keleigh Reynolds
#11/9/2022
#grab EPA files for COMID and weight

											
library(plyr)												
library(readr)
library(readxl)
library(arrow)
library(sf)
library(dplyr)	

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
    mutate(use=case_when(status %in% c("TS") ~TRUE,
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
  n = 17 ##of basins in the list 
  datalist = list()
  # or pre-allocate for slightly more efficiency
  datalist = vector("list", length = n)
  
  framesize_x<-df %>% 
    group_by(stratum,mdcaty) %>% 
    dplyr::summarise(n=sum(wgt)) #this should be the sum of each category - this is a sum to teh statewide frame - not by strata
  #need to filter by stratum and then adjust by stratum
  for (i in framesize_x$stratum) {
    print(i)
    df2<-df |> 
      filter(stratum %in% i)
    
    wgt<-df2$wgt
    wgtcat<-df2$mdcaty

    sites<-df2$use
  
    framesize_y<-framesize_x |> 
      filter(stratum %in% i)
    
    x1<-framesize_y$mdcaty
    x2<-framesize_y$n
    framesize<-setNames(x2,x1)
    
    x<-spsurvey::adjwgt(wgt,wgtcat,framesize,sites)

    df_adj<-cbind(df2,x) 
    df_adj<-df_adj |> select(wgt, x, COMID,mdcaty,SO, stratum,everything())
    datalist[[i]] <- df_adj # add it to your list
  }
  
  big_data = do.call(rbind, datalist)
}

prob_1_adjusted<-adjust_weight(prob_1)
prob_2_adjusted<-adjust_weight(prob_2)
prob_3_adjusted<-adjust_weight(prob_3)


#select columns from the dataframes
prob_1_short<-prob_1_adjusted %>% 
  dplyr::select(COMID,wgt,x,mdcaty,SO) %>% 
  dplyr::rename(adj_wgt=x) %>% 
  mutate(draw_year="2018_2022")

prob_2_short<-prob_2_adjusted %>% 
  dplyr::select(COMID,wgt,x,mdcaty,SO) %>% 
  dplyr::rename(adj_wgt=x) %>% 
  mutate(draw_year="2013_2018")

prob_3_short<-prob_3_adjusted %>% 
  dplyr::select(COMID,wgt,x,mdcaty,SO) %>% 
  dplyr::rename(adj_wgt=x) %>%  
  mutate(draw_year="2008_2013")


#bind them together
prob_all<-rbind(prob_1_short,prob_2_short,prob_3_short)

prob_all<-prob_all %>% 
  filter(adj_wgt >0) |> #take out any that did not get sampled
  distinct() #take out any that are duplicated-although there are, we may not want
#to do the draw year?
#look at the duplicates
 
dups <-prob_all[duplicated(prob_all$COMID)|duplicated(prob_all$COMID, fromLast=TRUE),]
dups
#so i thinnk we'll have to bind by draw year AND COMID, since they seem duplicated. 


write.csv(prob_all,"outputs/comid_adj_wgt_all_years.csv")

