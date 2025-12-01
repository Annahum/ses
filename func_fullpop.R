# create full pop

func_fullpop <- function(input_data, cr_inclusion){
  
cr_data <- {input_data}
  
cr_data_main <- cr_inclusion %>% 
    left_join(cr_data$cr_birthcountry_formatted , by = "lopnr") %>% 
    left_join(cr_data$cr_civil_formatted , by = "lopnr") %>% 
    left_join(cr_data$cr_migration_formatted, by = "lopnr") %>% 
    left_join(cr_data$cr_lisa_formatted , by = "lopnr") %>% 
    left_join(cr_data$cr_lisa_education_formatted , by = "lopnr") %>% 
    left_join(cr_data$cr_lisa_ssyk_formatted , by = "lopnr") %>% 
    
# for migration, assume that if missing, not present
  mutate(migration_3ybefore = ifelse (is.na(migration_3ybefore), 0, migration_3ybefore)) %>% 
  
# convert multiple variables to factor
    mutate(across(c(baseline_civil_status, baseline_education_length, baseline_employmentstatus, baseline_family_position,
                    baseline_family_type, baseline_family_overall, baseline_education_level, baseline_education_cat, baseline_region,
                    baseline_region, baseline_swedish_region, baseline_degurba_code, baseline_yseg, baseline_yseg_cat,
                    fodgreg5, birthcountry_cat, gender, income_disposable_3ymean_q5, income_disposable_cu_3ymean_q5, 
                    income_disposable_family_3ymean_q5, migration_3ybefore, moved_year_before, separation_3y, unemployed_3ybefore,
                    widow_3y, separation_widow_3y),  as.factor)) %>% 
  
# choose variables required for ses project, can be amended  
  select (lopnr,  enrol, age, gender, birthcountry_cat,        
            migration_3ybefore, baseline_swedish_region,  baseline_degurba_code, moved_year_before, 
            baseline_family_overall,  separation_widow_3y,  
            baseline_education_cat, baseline_education_length, 
            baseline_yseg_cat,  unemployed_3ybefore, 
            income_disposable_3ymean, income_disposable_3ymean_q5) %>% 

# one individual is missing birthcountry, re assign to sweden  
  mutate(birthcountry_cat = replace(birthcountry_cat, is.na(birthcountry_cat), "1")) %>% 
  
# not required currently but may be required for other projects:
# separation_3y, widow_3y,baseline_family_type, baseline_family_position, baseline_civil_status
#  baseline_education_level,baseline_yseg, year_interdat,fodgreg5, 
# baseline_swedish_region, baseline_employmentstatus, baseline_ssyk3_value, income_disposable_cu_3ymean,
# income_disposable_cu_3ymean_q5, income_disposable_family_3ymean,income_disposable_family_3ymean_q5, 
  
  return(cr_data_main)
}