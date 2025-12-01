#-----------------------------------------------------------------------------

# make validate clinical trial dataset - first the lopnrs must be accessed from a different file
# use age and sex variables from clinical dataset
# select variables that are also in table 1 of the trial

cr_validate_trial_raw <- read_sas("W:/C6_Berglund/data/rawdata/validate-trial/perpatient.sas7bdat")

cr_validate_pid <- read_sas("W:/C6_Berglund/data/rawdata/SCB Validate/validate_pid.sas7bdat")

cr_inclusion_validate <- cr_validate_trial_raw %>% 
  left_join(cr_validate_pid, by ="PID") %>%
  rename_all(tolower) %>% 
  select(pid, lopnr, interdat, rnd, stemi, d_gender, age) %>% 
  relocate(lopnr, .before = pid) %>% 

  mutate(enrol = ifelse( is.na(rnd), 1, 2))  %>% 
  filter(!is.na(lopnr)) %>% # leads to the loss of those that withdrew consent and ten others
  mutate(year_interdat = year(interdat)) %>% 
  select(-c(pid, rnd, interdat)) %>% 
  rename(gender=d_gender)


# nb completer variable definition
# Yes/
# Follow-up discontinued due to AE before 180 days/
# Deceased before 180 days/
# Withdrawn consent to all follow-up before 180 days/
# Lost to follow-up before 180 days