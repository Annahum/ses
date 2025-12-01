# full clinical populations as per table 1 in trial - done separately from main population derivation
# as different table ones in the two trials

# taste - choose clinical characteristics

cr_data_clin_taste <- read_sas("W:/C6_Berglund/data/rawdata/SCB TASTE/taste_lev_indata.sas7bdat") %>% 
  rename_all(tolower) %>% 
  select(lopnr, rndgroup, d_age_angiopci, d_gender, bmi=d_bmi, diabetes, smoking_status, hyperlip, hyperton, tidinf, 
         tidpci, tidcabg,  warfore, hepfore, trofore,
         ecgpcitm, symppcitm, killipklass, centreid) %>% 
  mutate_if(is.character, ~na_if(., ""))  %>% 
  mutate(enrol = ifelse(is.na(rndgroup), 1, 2)) %>% 
  mutate(centreid = substr(centreid, 1, 7)) %>% 
  filter(grepl("^SE", centreid))%>% 
  mutate(across(c( d_gender ,smoking_status, diabetes,hyperton, hyperlip,  warfore, hepfore, killipklass, 
                   tidcabg, tidinf, tidpci, trofore),  as.factor)) 

save(cr_data_clin_taste, file="W:/C6_Berglund/data/procdata/ses_trial_characteristics/taste/cr_data_clin_taste.Rda") 

#-------------------------------------------------------------------------------

# validate

# first choose the clinical characteristics

cr_validate_trial_raw <- read_sas("W:/C6_Berglund/data/rawdata/validate-trial/perpatient.sas7bdat")
cr_validate_pid <- read_sas("W:/C6_Berglund/data/rawdata/SCB Validate/validate_pid.sas7bdat")

# confirmed - the variable relating to MI type is randstrat (1 is stemi, 2 is nstemi) - stemi is not correct
# for the non enrolled population

cr_data_clin_validate <- cr_validate_trial_raw %>% 
  left_join(cr_validate_pid, by ="PID") %>%
  rename_all(tolower) %>% 
  filter(!is.na(lopnr)) %>% 
  select(lopnr, rnd, randstrat, d_gender, age,  bmi, weight_lo, smoking_status, diabetes, hyperton, hyperlip, 
         tidinf, tidpci, tidcabg, previous_stroke,  cpr_before_hospital,killipklass) %>% 
  mutate(across(c(d_gender, randstrat,  weight_lo, smoking_status, diabetes, hyperton, hyperlip, 
                tidinf, tidpci, tidcabg, previous_stroke, killipklass, cpr_before_hospital),  as.factor)) %>% 
  mutate(enrol = ifelse( is.na(rnd), 1, 2)) 

save(cr_data_clin_validate, file="W:/C6_Berglund/data/procdata/ses_trial_characteristics/validate/cr_data_clin_validate.Rda") 

rm(cr_validate_trial_raw, cr_validate_pid)

#-------------------------------------------------------------------------------

# Code to identify and choose variables that are in the trial, using the labels

# variable_labels <- var_label(cr_validate_trial_raw)

# matching_labels <- grep("före", variable_labels, value = TRUE, ignore.case = TRUE)

# print(matching_labels)


 
