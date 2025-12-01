# Three models:
# 1. Only clinical data,
# 2. Only socioeconomic
# 3. Clinical and socioeconomic data 

# Some minor processing is required:
# 1. Remove variables with too much missing data from clinical dataset,
# 2. Remove the variable for continuous income from se models so that do not have income in two forms in models
# 3. Make a combined clinical and socioeconomic model

# Taste:
cr_data_clin_taste_cv <- cr_data_clin_taste %>% 
  select(lopnr, enrol, d_age_angiopci, d_gender, tidpci, tidcabg, warfore, hepfore, trofore)

cr_data_se_taste_cv <- cr_data_se_taste %>% 
  select(-income_disposable_3ymean)

# make taste full population with clinical and se characteristics
cr_data_full_taste_cv <- cr_data_se_taste %>% 
  left_join(select(cr_data_clin_taste,lopnr, tidpci, tidcabg, warfore, hepfore, trofore), by = "lopnr") %>% #remove age and sex as in se data
  select(-income_disposable_3ymean)

# Validate
cr_data_clin_validate_cv <- cr_data_clin_validate %>% 
  select(lopnr, enrol, d_gender, age, randstrat, tidpci, tidcabg) 

cr_data_se_validate_cv <- cr_data_se_validate %>% 
  select(-income_disposable_3ymean)

# make full population with clinical and se characteristics 
cr_data_full_validate_cv <- cr_data_se_validate %>% 
  left_join(select(cr_data_clin_validate,lopnr,  randstrat, tidpci, tidcabg), by = "lopnr") %>% #remove age and sex as in se data
  select(-income_disposable_3ymean)