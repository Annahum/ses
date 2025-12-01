# import taste clinical data - also use age and sex variables

cr_inclusion_taste <- read_sas("W:/C6_Berglund/data/rawdata/SCB TASTE/taste_lev_indata.sas7bdat") %>% 
  rename_all(tolower) %>% 
  
# select required variables, and the table 1 variables from the trial
  select(lopnr, centreid, rndgroup, interdat, d_age_angiopci, d_gender) %>% 
  mutate(year_interdat = year(interdat)) %>% 
  mutate_if(is.character, ~na_if(., ""))  %>% 
  
# remove final three characters from centreid
  mutate(centreid = substr(centreid, 1, 7)) %>% 
  
# remove non swedish centres
  filter(grepl("^SE", centreid))%>% 
  
# create new variable to show if randomized or not
  mutate(enrol = ifelse(is.na(rndgroup), 1, 2)) %>% 

# remove variables not required
  select(-c(interdat, rndgroup, centreid)) %>% 
  
# rename
  rename(age = d_age_angiopci, gender = d_gender)


#----------------------------------------------------------------------------------------------------------------------

        

