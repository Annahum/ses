# full covariate formatting

# the data from scb is collected at the end of each year
# therefore to ensure temporality, remove all observations occuring the same year as the index interdate date or later

func_formatting <- function(input_data, cr_inclusion){
  
  cr_data <- {input_data}
  
#-------------------------------------------------------------------------------
#civil data
  
cr_civil_formatted <- select(cr_inclusion, lopnr, year_interdat) %>% 
  left_join(cr_data$cr_civil, by = "lopnr") %>% 
    
  mutate(civil_status = case_when(civil == "OG" ~ 1, # "OG" single
                                    civil == "G" ~ 2,  # "G" Married --> combine with registered partner
                                    civil == "RP" ~ 2,  # "RP" Registered partner --> combine with married
                                    civil == "SP" ~ 3,  # "S" Separated partner --> combine with divorced 
                                    civil == "S" ~ 3,  # "S" Divorced --> combine with Separated partner
                                    civil == "Ä" ~ 4,  #"Ä" Widow --> combine with surviving partner
                                    civil == "EP" ~ 4)) %>%   # surviving partner --> combine with widow 
    
#restrict to three years before
  filter(year < year_interdat) %>% 
  filter(year_interdat - year <=3) %>% 
    
# identify if any individuals have multiple observations for one year which have different data for civil status
  group_by(lopnr, year) %>%
  mutate(status_discrepancy = n_distinct(civil_status) > 1) %>%
  ungroup() %>% 
  select(-(status_discrepancy)) %>% 
    
# identify baseline civil status
  group_by(lopnr) %>%
  mutate(baseline_civil_status = civil_status[which.min(abs(year - year_interdat))]) %>% 
  ungroup() %>% 
    
# identify separation in the last three years
  group_by(lopnr) %>%
  arrange(lopnr, year) %>%
  mutate(separation_3y = ifelse(lag(civil_status) == 2 & civil_status == 3, 1, NA))  %>% 
  ungroup()  %>% 
    
# identify partner death in the last three years
  group_by(lopnr) %>%
  arrange(lopnr, year) %>%
  mutate(widow_3y = ifelse(lag(civil_status) == 2 & civil_status == 4, 1, NA)) %>% 
  ungroup()  %>% 
  
# ensure present all rows and relabel with 0 otherwise, and make composite of death or separation
  group_by(lopnr) %>% 
  
  mutate(
    separation_3y = case_when(
      any(separation_3y == 1, na.rm = TRUE) ~ 1,
      is.na(baseline_civil_status) ~ NA_real_,
      TRUE ~ 0
    ),
    widow_3y = case_when(
      any(widow_3y == 1, na.rm = TRUE) ~ 1,
      is.na(baseline_civil_status) ~ NA_real_,
      TRUE ~ 0
    ),
    separation_widow_3y = case_when(
      separation_3y == 1 | widow_3y == 1 ~ 1,
      is.na(baseline_civil_status) ~ NA_real_,
      TRUE ~ 0
    )
  ) %>%
  ungroup()

# use other method to check divorce and spousal death captured
  
cr_civil_formatted <- cr_civil_formatted[order(cr_civil_formatted$lopnr, cr_civil_formatted$year), ]
cr_civil_formatted$change_detected_seperation <- with(cr_civil_formatted, ave(civil_status, lopnr, FUN = function(x) any(diff(x) == 1 & x[-length(x)] == 2 & x[-1] == 3)))
  
cr_civil_formatted <- cr_civil_formatted[order(cr_civil_formatted$lopnr, cr_civil_formatted$year), ]
cr_civil_formatted$change_detected_widow <- with(cr_civil_formatted, ave(civil_status, lopnr, FUN = function(x) any(diff(x) == 2 & x[-length(x)] == 2 & x[-1] == 4)))
  
# make one row per participant and choose only required variables
cr_civil_formatted <- select(cr_civil_formatted, lopnr, baseline_civil_status, separation_3y, widow_3y, separation_widow_3y) %>% 
  group_by(lopnr) %>% 
  filter(row_number()==1) %>% 
  ungroup()
  
#-------------------------------------------------------------------------------  
# birthcountry, recategorise due to small numbers (note not using demography file)
  
cr_birthcountry_formatted <- select(cr_inclusion, lopnr) %>% 
  left_join(cr_data$cr_birthcountry, by = "lopnr") %>% 
  mutate(birthcountry_cat = case_when(fodgreg5 == "Sverige" ~ 1,
                                      fodgreg5 == "Norden utom Sverige" ~ 2,
                                      fodgreg5 == "EU27 utom Norden" ~ 3,
                                      fodgreg5 == "Europa utom EU27 och Norden" ~ 3,
                                      fodgreg5 == "Nordamerika" ~ 4,
                                      fodgreg5 == "Sydamerika" ~ 4,
                                      fodgreg5 == "Afrika" ~ 5,
                                      fodgreg5 == "Asien" ~ 5,
                                      fodgreg5 == "Sovjetunionen" ~ 5,
                                      fodgreg5 == "Oceanien" ~ 5,
                                      fodgreg5 == "Okänt" ~ NA,
                                      TRUE ~ NA))
  
#-------------------------------------------------------------------------------  
  
# lisa part 1 formatting (not all variables used due to updates in more recent data (education))
cr_lisa_formatted <- select(cr_inclusion, lopnr, year_interdat) %>% 
  left_join(cr_data$cr_lisa, by ="lopnr") %>% 
    
# ensure right time frames
  filter(year < year_interdat) %>% 
  filter(year_interdat - year <=3) %>% 
    
# identify if any individuals have multiple observations for one year which have different data for key variables
  group_by(lopnr, year) %>%
  mutate(status_discrepancy2 = n_distinct(alosdag) > 1) %>%
  mutate(status_discrepancy3 = n_distinct(famstf) > 1) %>%
  mutate(status_discrepancy4 = n_distinct(famtypf) > 1) %>%
  mutate(status_discrepancy5 = n_distinct(antflyttkommun) > 1) %>%
  mutate(status_discrepancy6 = n_distinct(kommun) > 1) %>%
  ungroup() %>% 
  select(-(starts_with("status_discrepancy"))) %>% 
    
# format disposable income (varying variable over time, with change after 2004) 
# note for consumption unit is derived prior to 2004
# prioritise the later years if duplications in values present
 mutate(
    income_disposable = coalesce(dispink04, dispink),
    income_disposable_family = coalesce(dispinkfam04, dispinkfam),
    income_disposable_cu = coalesce(dispinkke04, (dispinkfam / konsviktf))
   ) %>% 
   select(-c(starts_with("disp"), konsviktf, konsviktf04)) %>% 
    
# format employment status (varying variable over time, with overlap in the transition years, and ensure
# that prioritise the later year)
    
 mutate(employment_status = 
         as.numeric( coalesce(syssstat11, syssstatj, syssstat, syssstatg))) %>%
 select(-c(starts_with("sysss"))) %>% 
  
# get mean three year disposable income value
  group_by(lopnr) %>% 
    mutate(income_disposable_3ymean = mean(income_disposable, na.rm = TRUE)) %>% 
    mutate(income_disposable_family_3ymean = mean(income_disposable_family, na.rm = TRUE)) %>% 
    mutate(income_disposable_cu_3ymean = mean(income_disposable_cu, na.rm = TRUE)) %>%
    ungroup() %>% 
    
# due to small numbers convert number of days unemployed to a binary variable if have been unemployed
# in the last three years 
  group_by(lopnr) %>% 
  mutate(days_unemployed_3years_before = sum(alosdag, na.rm = TRUE)) %>% 
  mutate(unemployed_3ybefore = ifelse(days_unemployed_3years_before >= 1, 1, 0)) %>% 
  ungroup() %>% 
    
# format family position
  mutate(family_position = 
       (case_when( 
        grepl("^1", famstf) ~ 1, # Married/cohabiting/partnership
        grepl("^2", famstf) ~ 2, # Single w.children
        grepl("^3", famstf) ~ 3, # Child to 1/2 parent(s)
        grepl("^4", famstf) ~ 4, # Other single
        grepl("^0", famstf) ~ 5, # Unknown
        TRUE ~ NA )) )  %>% # Unknown
  
# further format family position into fewer categories
  mutate(family_overall = case_when(
    family_position == 4 ~ 1, # Single
    family_position == 3 ~ 1, # Single, reclassification of children living at home as single
    family_position == 2 ~ 2, # Lone parent (single with children)
    family_position == 1 ~ 3, # Married/cohabiting/partnership
    family_position == 5 ~ 4  # Unknown
    )) %>%
    
# format family position and type into composite measure 
  mutate(family_type= case_when(
    family_position == 1 & famtypf %in% c(11, 21) ~ 1,         #Married/cohabiting/partnership without children living at home  
    family_position == 1 & famtypf %in% c(12, 13, 22, 23) ~ 2, #Married/cohabiting/partnership with children living at home <18/>=18yrs
    family_position == 2 & famtypf %in% c(31, 32, 41, 42) ~ 3, #Single with children living at home <18/>=18yrs
    family_position == 3 & famtypf >= 12 & famtypf <= 42 ~ 4,  #Child living at home </>=18yrs (biologic/adopted/other) to both/one/single parent(s)
    family_position == 4 & famtypf == 50 ~ 5, #Other single 
    family_position == 5 & famtypf == 0 ~ 6, #People with incomplete/contradictory information
    TRUE ~ NA)) %>% 

# identify baseline (ie last recorded) family position, type
  group_by(lopnr) %>%
  mutate(baseline_family_position = family_position[which.min(abs(year - year_interdat))]) %>% 
  mutate(baseline_family_overall = family_overall[which.min(abs(year - year_interdat))]) %>% 
  mutate(baseline_family_type = family_type[which.min(abs(year - year_interdat))]) %>% 
  ungroup()  %>% 
    
# format kommun of residence (only first two digits are required)    
  mutate(region_number = substr(kommun, 1, 2)) %>% 
  mutate(region = case_when(region_number == "01" ~ 1,  #Stockholms 
                            region_number == "03" ~ 2,  #Uppsala 
                            region_number == "04" ~ 3,  #Södermanlands 
                            region_number == "05" ~ 4,  #Östergötlands 
                            region_number == "06" ~ 5,  #Jönköpings 
                            region_number == "07" ~ 6,  #Kronobergs 
                            region_number == "08" ~ 7,  #Kalmar 
                            region_number == "09" ~ 8,  #Gotlands 
                            region_number == "10" ~ 9,  #Blekinge 
                            region_number == "12" ~ 10, #Skåne 
                            region_number == "13" ~ 11, #Hallands 
                            region_number == "14" ~ 12, #Västra Götalands 
                            region_number == "17" ~ 13, #Värmlands 
                            region_number == "18" ~ 14, #Örebro 
                            region_number == "19" ~ 15, #Västmanlands 
                            region_number == "20" ~ 16, #Dalarnas 
                            region_number == "21" ~ 17, #Gävleborgs 
                            region_number == "22" ~ 18, #Västernorrlands 
                            region_number == "23" ~ 19, #Jämtlands 
                            region_number == "24" ~ 20, #Västerbottens 
                            region_number == "25" ~ 21, #Norrbottens 
                            TRUE ~ NA )) %>% 
    
# identify baseline (ie last recorded) kommun
  group_by(lopnr) %>%
  mutate(baseline_region = region[which.min(abs(year - year_interdat))]) %>% 
  ungroup()   %>% 

# identify baseline (ie last recorded) region (one of six regions)   
  mutate(swedish_region = case_when(
    substr(kommun, 1, 2) %in% c('01', '09') ~ 1, #Stockholm
    substr(kommun, 1, 2) %in% c('03', '04', '17', '18', '19', '20', '21') ~ 2, #Uppsala
    substr(kommun, 1, 2) %in% c('05', '06', '08') ~ 3, #Linkoping
    substr(kommun, 1, 2) %in% c('07', '10', '11', '12') | kommun %in% c('1315', '1380', '1381') ~ 4, #Lund_malmo
    substr(kommun, 1, 2) %in% c('14', '15', '16') | kommun %in% c('1382', '1383', '1384') ~ 5, #Gothenberg
    substr(kommun, 1, 2) %in% c('22','23', '24', '25') ~ 5, #Umea
    TRUE ~ NA )) %>% 
  
# identify baseline (ie last recorded) swedish region
  group_by(lopnr) %>%
  mutate(baseline_swedish_region = swedish_region[which.min(abs(year - year_interdat))]) %>% 
  ungroup()   %>% 
  
  # identify baseline (ie last recorded) level of urbaisation
  group_by(lopnr) %>%
  mutate(baseline_degurba_code = degurba_code[which.min(abs(year - year_interdat))]) %>% 
  ungroup()   %>% 
    
# due to small numbers convert move_kommun to a binary variable if have moved in the last year
  group_by(lopnr) %>% 
  mutate(moves_year_before = antflyttkommun[which.min(abs(year - year_interdat))]) %>% 
  mutate(moved_year_before = ifelse(moves_year_before>=1, 1, moves_year_before)) %>% 
  ungroup() %>%
    
# identify baseline (ie last recorded) employment status
  group_by(lopnr) %>%
  mutate(baseline_employmentstatus = employment_status[which.min(abs(year - year_interdat))]) %>% 
  ungroup()   %>% 
    
# make one row per individual
  group_by(lopnr) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>%
    
# make quintiles of income
  mutate(income_disposable_3ymean_q5 = ntile(income_disposable_3ymean, 5)) %>% 
  mutate(income_disposable_family_3ymean_q5 = ntile(income_disposable_family_3ymean, 5)) %>% 
  mutate(income_disposable_cu_3ymean_q5 = ntile(income_disposable_cu_3ymean, 5)) %>% 
  
#select required variables
  select(lopnr,  baseline_family_position, baseline_family_type, baseline_region, baseline_family_overall, 
         baseline_swedish_region, baseline_degurba_code, moved_year_before,  
         unemployed_3ybefore, income_disposable_3ymean, income_disposable_3ymean_q5,  income_disposable_family_3ymean,
           income_disposable_family_3ymean_q5, income_disposable_cu_3ymean, income_disposable_cu_3ymean_q5, baseline_employmentstatus)
  
#-------------------------------------------------------------------------------  
  
#format migration data
cr_migration_formatted <- select(cr_inclusion, lopnr,  year_interdat) %>%
  left_join(cr_data$cr_migration, by = "lopnr") %>% 
    
# ensure the right time period, and three year look back
  filter(!is.na(migration_date)) %>% 
  mutate(migration_year = year(migration_date)) %>% 
  filter(migration_year< year_interdat) %>% 
  filter(year_interdat - migration_year <=3) %>% 
    
# identify immigration to Sweden
  filter(posttyp == "Inv") %>% 
    
# one row per individual
  group_by(lopnr) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  mutate(migration_3ybefore = 1) %>% 
  select(lopnr, migration_3ybefore)
  
#-------------------------------------------------------------------------------  
  
cr_lisa_education_formatted <- select(cr_inclusion, lopnr,  year_interdat) %>%
    left_join(cr_data$cr_lisa_education, by ="lopnr") %>% 
    
# ensure right time frames
   filter(year < year_interdat) %>% 
   filter(year_interdat - year <=3) %>% 
    
# format education, convert to number format, and deal with missing
 mutate(education_length = case_when(
      sun2000niva_old_complete >= 1 & sun2000niva_old_complete <= 2 ~ 1, #1 "Primary education <=9yrs" 
      sun2000niva_old_complete >= 3 & sun2000niva_old_complete <= 4 ~ 2, #2 "Secondary education <=3yrs"
      sun2000niva_old_complete >= 5 & sun2000niva_old_complete <= 7 ~ 3, #3 "Tertiary education </>3yrs" 
      TRUE ~ NA)) %>%  
    
# format highest level of education (contains three numbers, then a letter. First number key for categorisation)
 mutate(highest_education_level = 
             case_when( 
               grepl("^0", sun2000inr) ~ 0, #General education
               grepl("^1", sun2000inr) ~ 1, #Pedagogical/teaching
               grepl("^2", sun2000inr) ~ 2, #Humanities/Art
               grepl("^3", sun2000inr) ~ 3, #Social science/Law/Business
               grepl("^4", sun2000inr) ~ 4, #Science/Math/Data
               grepl("^5", sun2000inr) ~ 5, #Technology/Manufacturing
               grepl("^6", sun2000inr) ~ 6, #Agriculture/Gardening/Forestry
               grepl("^7", sun2000inr) ~ 7, #Health- and social care
               grepl("^8", sun2000inr) ~ 8, #Services
               grepl("^9", sun2000inr) ~ NA, #Unknown
               TRUE ~ NA )) %>% 
    
 mutate(highest_education_cat =
             case_when(
               highest_education_level == 0 ~ 1, #general
               highest_education_level %in% c(1,2,3) ~ 2, #social science
               highest_education_level %in% c(4,5,6,7) ~ 3, #science, technology, agriculture, health
               highest_education_level == 8 ~ 4 #services
                     )) %>% 
    
# identify baseline (ie last recorded) education 
  group_by(lopnr) %>%
  mutate(baseline_education_length = education_length[which.min(abs(year - year_interdat))]) %>% 
  mutate(baseline_education_level = highest_education_level[which.min(abs(year - year_interdat))]) %>% 
  mutate(baseline_education_cat = highest_education_cat[which.min(abs(year - year_interdat))]) %>% 
  ungroup() %>% 
    
# make one row per individual
    group_by(lopnr) %>% 
    filter(row_number()==1) %>% 
    ungroup() %>% 
    select(lopnr, baseline_education_length, baseline_education_level, baseline_education_cat)
  
#-------------------------------------------------------------------------------
# occupational status (note sei is not used as only available in 1990)
cr_lisa_ssyk_formatted <- select(cr_inclusion, lopnr,  year_interdat) %>%
  left_join(cr_data$cr_lisa_ssyk, by ="lopnr") %>% 
    
# ensure right time frames
  filter(year < year_interdat) %>% 
  filter(year_interdat - year <=3) %>% 
    
# identify baseline (ie last recorded) ssyk
  group_by(lopnr) %>%
  mutate(baseline_yseg = yseg_complete[which.min(abs(year - year_interdat))]) %>% 
  mutate(baseline_ssyk3 = ssyk3[which.min(abs(year - year_interdat))]) %>% 
  ungroup() %>% 
    
# make one row per individual
  group_by(lopnr) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  
# recategorise yseg
  
mutate(baseline_yseg_cat = 
  case_when(baseline_yseg %in% c("01", "02") ~ 1, #Senior Officials and Managers Professionals
  baseline_yseg %in% c("04", "05") ~ 2, #Small Business Owners Excluding Farmers, Farmers
  baseline_yseg %in% c("06", "08") ~ 3, #Supervisors and Technicians, Skilled Manual Workers
  baseline_yseg %in% c("03", "07") ~ 4, #Other Office Employees, Skilled Workers in Trade, Services, and Care
  baseline_yseg %in% c("09") ~ 5, #Other Manual Worker
  baseline_yseg %in% c("10") ~ 6, # Unemployed or no occupational status
  TRUE ~ NA)) %>% 
  
# values are 01 - Senior Officials and Managers, 02 Professionals,
# 03 Other Office Employees, 04 Small Business Owners Excluding Farmers,
# 05 - Farmers, 06 Supervisors and Technicians, 07 -Skilled Workers in Trade, Services, and Care",
# 08 - Skilled Manual Workers, 09 Other Manual Workers, 10 Unemployed
    
# make ten groups based on baseline_ssyk3 by choosing first number
  mutate(baseline_ssyk3_value=substr(baseline_ssyk3, 1, 1))  %>% 
  select(lopnr, baseline_yseg, baseline_yseg_cat, baseline_ssyk3_value) 
  
#-------------------------------------------------------------------------------
  
cr_covariate <- list(
    cr_civil_formatted = cr_civil_formatted,
    cr_birthcountry_formatted = cr_birthcountry_formatted,
    cr_lisa_formatted = cr_lisa_formatted,
    cr_migration_formatted = cr_migration_formatted,
    cr_lisa_education_formatted = cr_lisa_education_formatted,
    cr_lisa_ssyk_formatted = cr_lisa_ssyk_formatted
  )
  
}