#fetching bug data for Matt

bugs<-fetch::fetch_bugs(output = "all")
bugs<-fetch::fetch_bugs(output = "standard")

lakes_raw<-bugs$raw_bugs %>% 
  filter(grepl("*-0.0", MSDH_LINKED_ID_VALIDATOR, ignore.case = TRUE))

bug_method<-bugs$bug_method

lakes_raw2<-merge(lakes_raw,bug_method,
                      by.x = "MSDH_LINKED_ID_VALIDATOR",
                      by.y= "MSSIH_LINKED_ID_VALIDATOR")

lake_list<-unique(lakes_raw$MSDH_LINKED_ID_VALIDATOR) #this is the comprehensive
#list of the lakes in the raw, i need to find the ones missing

missing<-setdiff(lakes_raw$MSDH_LINKED_ID_VALIDATOR,
                 lakes_raw2$MSDH_LINKED_ID_VALIDATOR)

missing<-lakes_raw2 %>% 
  filter(!MSDH_LINKED_ID_VALIDATOR %in% lake_list )

missing_info<-lakes_raw %>% 
  filter(MSDH_LINKED_ID_VALIDATOR %in% missing) %>% 
  group_by(MSDH_LINKED_ID_VALIDATOR) %>% 
  summarise(total = sum(MSDH_INDIVIDUAL_SPECIES_CNT)) #now from here we can 
#correct the validator to the correct methods

write.csv(lakes_raw, "outputs/lakes_raw_grab_10_26_23.csv")         
