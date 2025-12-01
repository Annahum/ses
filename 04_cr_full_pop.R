# make full populations

# taste
cr_data_se_taste <- func_fullpop(input_data=cr_cov_formatted_taste, cr_inclusion =  cr_inclusion_taste)
save(cr_data_se_taste, file="W:/C6_Berglund/data/procdata/ses_trial_characteristics/taste/cr_data_se_taste.Rda") 

# validate
cr_data_se_validate <- func_fullpop(input_data=cr_cov_formatted_validate, cr_inclusion =  cr_inclusion_validate)
save(cr_data_se_validate, file="W:/C6_Berglund/data/procdata/ses_trial_characteristics/validate/cr_data_se_validate.Rda") 

#-------------------------------------------------------------------------------
# subgroups

# 1. male and female

#taste
cr_data_se_taste_male <- cr_data_se_taste %>% 
  filter(gender==1)
save(cr_data_se_taste_male, file="W:/C6_Berglund/data/procdata/ses_trial_characteristics/taste/cr_data_se_taste_male.Rda") 

cr_data_se_taste_female <- cr_data_se_taste  %>% 
  filter(gender==2)
save(cr_data_se_taste_female, file="W:/C6_Berglund/data/procdata/ses_trial_characteristics/taste/cr_data_se_taste_female.Rda") 


#validate
cr_data_se_validate_male <- cr_data_se_validate %>% 
  filter(gender==1)
save(cr_data_se_validate_male, file="W:/C6_Berglund/data/procdata/ses_trial_characteristics/validate/cr_data_se_validate_male.Rda") 

cr_data_se_validate_female <- cr_data_se_validate  %>% 
  filter(gender==2)
save(cr_data_se_validate_female, file="W:/C6_Berglund/data/procdata/ses_trial_characteristics/validate/cr_data_se_validate_female.Rda") 

#-------------------------------------------------------------------------------
# 2. age

#taste
cr_data_se_taste_younger <- cr_data_se_taste %>% 
  filter(age<65)

save(cr_data_se_taste_younger, file="W:/C6_Berglund/data/procdata/ses_trial_characteristics/taste/cr_data_se_taste_younger.Rda") 

cr_data_se_taste_older <- cr_data_se_taste  %>% 
  filter(age>=65)

save(cr_data_se_taste_older, file="W:/C6_Berglund/data/procdata/ses_trial_characteristics/taste/cr_data_se_taste_older.Rda") 

#validate
cr_data_se_validate_younger <- cr_data_se_validate %>% 
  filter(age<65)

save(cr_data_se_validate_younger, file="W:/C6_Berglund/data/procdata/ses_trial_characteristics/validate/cr_data_se_validate_younger.Rda") 

cr_data_se_validate_older <- cr_data_se_validate  %>% 
  filter(age>=65)

save(cr_data_se_validate_older, file="W:/C6_Berglund/data/procdata/ses_trial_characteristics/validate/cr_data_se_validate_older.Rda") 

rm(cr_cov_formatted_taste, cr_cov_formatted_validate, cr_inclusion_taste, cr_inclusion_validate, func_fullpop)
