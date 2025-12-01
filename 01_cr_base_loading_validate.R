# importing data and basic formatting

#-------------------------------------------------------------------------------
# import civil data, and create new variable that identifies the year the data is from

years <- 1990:2019
file_names <- paste0("W:/C6_Berglund/data/rawdata/SCB VALIDATE/validate_lev_civil_", years, ".sas7bdat")

data_list_civil <- lapply(seq_along(file_names), function(i) {
  df <- read_sas(file_names[i])
  df$year <- years[i] # Add the year variable
  return(df)
})

# Bind all the data frames together
cr_civil <- bind_rows(data_list_civil) %>% 
  rename_all(tolower) %>% 
  select(lopnr, civil, year) 

rm(data_list_civil)

#-------------------------------------------------------------------------------

# import demographic data 
# note several patients have different years of birth listed, therefore use clinical datafile for age
# also there are a number of different values for sex for several individuals, also use clinical data

cr_demographic <- read_sas("W:/C6_Berglund/data/rawdata/SCB VALIDATE/validate_lev_demografi.sas7bdat") %>% 
  rename_all(tolower) %>% 
  select(lopnr, varldsdelnamn) %>% 
  mutate(varldsdelnamn=ifelse(varldsdelnamn == "", NA, varldsdelnamn)) %>%
  
# there are also some duplications where some individuals have several records (due to missing values for 
# birth country in one row)
  filter(!is.na(varldsdelnamn)) 

# demonstrate issue with year of birth
cr_demographic_issue <- read_sas("W:/C6_Berglund/data/rawdata/SCB VALIDATE/validate_lev_demografi.sas7bdat") %>% 
  rename_all(tolower) %>% 
  group_by(lopnr) %>%
  mutate(status_discrepancy1 = n_distinct(fodar) > 1) %>%
  mutate(status_discrepancy2 = n_distinct(kon) > 1) %>%
  ungroup()  

rm(cr_demographic_issue)

#-------------------------------------------------------------------------------

# import newer country of birth data

cr_birthcountry <-  read_sas("W:/C6_Berglund/data/rawdata/SCB VALIDATE/FodLand 2024-11-29/validate_lev_fodland_eu27_2020.sas7bdat") %>% 
  rename_all(tolower) %>% 
  select(lopnr, fodgreg5)

#-------------------------------------------------------------------------------

# import lisa data part 1

#import datafile that contains the size of region
region_size <-  read_sas("W:/C6_Berglund/data/rawdata/SCB VALIDATE/degurba.sas7bdat")  %>% 
  rename_all(tolower)  

years <- 1990:2019
file_names <- paste0("W:/C6_Berglund/data/rawdata/SCB VALIDATE/validate_lev_lisa_", years, ".sas7bdat")

data_list_lisa <- lapply(seq_along(file_names), function(i) {
  df <- read_sas(file_names[i])
  df$year <- years[i] # Add the year variable
  return(df)
})

cr_lisa <- bind_rows(data_list_lisa) %>% 
  rename_all(tolower) %>% 
  relocate(year, .after = lopnr) %>% 
  
# replace all the empty strings ("") with NA
  mutate_if(is.character, ~na_if(., ""))  %>% 
  
  # add in data on rural/urban
  left_join(select(region_size, kommun, degurba_code), by = "kommun")

rm(data_list_lisa, region_size)

#-------------------------------------------------------------------------------

# migration - note location within SCB taste folder
cr_migration <-  read_sas("W:/C6_Berglund/data/rawdata/SCB TASTE/Migrationer_240926/validate_lev_migr_240926.sas7bdat") %>% 
  rename_all(tolower) %>% 
  
# replace all the empty strings ("") with NA
  mutate_if(is.character, ~na_if(., "")) %>% 
  
# remove varldsdelnamn, present in other newer data sources
  select(-(varldsdelnamn)) %>% 
  
# convert to date format
  mutate(migration_date = ymd(datum)) 

#-------------------------------------------------------------------------------

# import newer lisa data containing new ssyk, yseg variables
# and more complete education data (sun2000)
# (note same file name as original lisa data, but different location!)

years <- 1990:2022
file_names <- paste0("W:/C6_Berglund/data/rawdata/SCB VALIDATE/validate_Utbildning/validate_lev_lisa_", years, ".sas7bdat")

data_list_lisa_utbildning <- lapply(seq_along(file_names), function(i) {
  df <- read_sas(file_names[i])
  df$year <- years[i] # Add the year variable
  return(df)
})

cr_lisa_utbildning <- bind_rows(data_list_lisa_utbildning)  %>% 
  
  # replace all the empty strings ("") with NA
  mutate_if(is.character, ~na_if(., "")) %>%  
  
  # replace all values starting with * with NA 
  mutate(across(everything(), ~ ifelse(grepl("^\\*", .), NA, .))) 

# education - do not use Sun2020Niva_Old or Sun2020INR as contains data from 2019 to 2022
# create a composite sun2000niva_old - first confirm it is the same variable over time with different capitalisation

education_check <- cr_lisa_utbildning %>% 
  select(LopNr, year, Sun2000niva_old, Sun2000niva_Old, Sun2000Niva_old, Sun2020Niva_Old) %>% 
  mutate(across(c(Sun2000niva_old, Sun2000niva_Old, Sun2000Niva_old, Sun2020Niva_Old), ~ ifelse(is.na(.), ., 1))) %>% 
  mutate(across(c(Sun2000niva_old, Sun2000niva_Old, Sun2000Niva_old, Sun2020Niva_Old), as.numeric)) %>% 
  mutate(row_sum = rowSums(across(c(Sun2000niva_old, Sun2000niva_Old, Sun2000Niva_old, Sun2020Niva_Old)), na.rm = TRUE))

rm(education_check)

cr_lisa_education <- cr_lisa_utbildning %>% 
  mutate(sun2000niva_old_complete = coalesce(Sun2000niva_old, Sun2000niva_Old, Sun2000Niva_old, Sun2020Niva_Old)) %>% 
  select(LopNr, year, sun2000niva_old_complete, Sun2000Inr) %>% 
  rename_all(tolower) %>% 
  filter(!is.na(sun2000niva_old_complete)|!is.na(sun2000inr))

# the dataset contains two additional year variables, syskar and syskar_j16. These show the most recent data that the records have, 
# and when it came from for each year for the corresponding variables. Remove these year variables, and use the last information available
# for that year, but, first remove data from the future. J16 came in from 2010 and so will not be used
cr_lisa_ssyk <- cr_lisa_utbildning %>% 
  mutate(SsykAr= as.numeric(SsykAr)) %>% 
  mutate(yseg_complete = coalesce( YSEG, YSeG)) %>% #again, different capitalisations
  filter(is.na(SsykAr) | SsykAr <= year) %>% 
  select(LopNr, year, Ssyk3,  yseg_complete) %>% 
  filter(!is.na(Ssyk3)|!is.na(yseg_complete)) %>% 
  rename_all(tolower)

rm(data_list_lisa_utbildning, cr_lisa_utbildning)  

#-------------------------------------------------------------------------------
#make list

cr_validate_base <- list(
  cr_civil = cr_civil,
  cr_demographic = cr_demographic,
  cr_birthcountry = cr_birthcountry,
  cr_lisa = cr_lisa,
  cr_migration = cr_migration,
  cr_lisa_education = cr_lisa_education,
  cr_lisa_ssyk = cr_lisa_ssyk
)

rm(cr_civil, cr_demographic, cr_birthcountry, cr_lisa, cr_migration, cr_lisa_education, cr_lisa_ssyk)
