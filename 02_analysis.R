#------------------------------------------------------------------------------
# Analysis script

# Part 1 - generation of baseline tables

# Taste

# taste main (socioeconomic)
func_tableone(input_data = cr_data_se_taste, location_name ="outputs/taste", txt_name ="tableone_taste.txt")

# taste main (socioeconomic) showing missing per group
func_tableone(input_data = cr_data_se_taste, location_name ="outputs/taste", txt_name ="tableone_taste_missing.txt", convert_na_to_factor=T)

# taste subgroups
func_tableone(input_data = cr_data_se_taste_older, location_name ="outputs/taste", txt_name ="tableone_taste_older.txt")
func_tableone(input_data = cr_data_se_taste_younger, location_name ="outputs/taste", txt_name ="tableone_taste_younger.txt")
func_tableone(input_data = cr_data_se_taste_male, location_name ="outputs/taste", txt_name ="tableone_taste_male.txt")
func_tableone(input_data = cr_data_se_taste_female, location_name ="outputs/taste", txt_name ="tableone_taste_female.txt")

# taste clinical
func_tableone_clin(input_data = cr_data_clin_taste, taste = T, nonnormalvar = c("ecgpcitm","symppcitm"), location_name ="outputs/taste", txt_name ="tableone_clin_taste.txt")

rm(cr_data_se_taste_older, cr_data_se_taste_younger, cr_data_se_taste_male, cr_data_se_taste_female)

#-------------------------------------------------------------------------------

# Validate

# validate main (socioeconomic)
func_tableone(input_data = cr_data_se_validate, location_name ="outputs/validate", txt_name ="tableone_validate.txt")

# validate main (socioeconomic) showing missing per group
func_tableone(input_data = cr_data_se_validate, location_name ="outputs/validate", txt_name ="tableone_validate_missing.txt", convert_na_to_factor=T)

# validate subgroups
func_tableone(input_data = cr_data_se_validate_older, location_name ="outputs/validate", txt_name ="tableone_validate_older.txt")
func_tableone(input_data = cr_data_se_validate_younger, location_name ="outputs/validate", txt_name ="tableone_validate_younger.txt")
func_tableone(input_data = cr_data_se_validate_male, location_name ="outputs/validate", txt_name ="tableone_validate_male.txt")
func_tableone(input_data = cr_data_se_validate_female, location_name ="outputs/validate", txt_name ="tableone_validate_female.txt")

# validate clinical, and then showing missing
func_tableone_clin(input_data = cr_data_clin_validate, taste = F,  nonnormalvar = c("age", "bmi"), location_name ="outputs/validate", txt_name ="tableone_clin_validate.txt")

rm(cr_data_se_validate_older, cr_data_se_validate_younger, cr_data_se_validate_male, cr_data_se_validate_female)

#-------------------------------------------------------------------------------

# Part 2 - random forest predictive model

#-------------------------------------------------------------------------------
# Run random forests

# 1. Taste
func_randomforest_10cv_hyper(input_data = cr_data_clin_taste_cv,  
                     complete_case = F,
                     location_name1 = "outputs/taste/taste_clin_auc_catcont_10cv_hyper.csv")

func_randomforest_10cv_hyper(input_data = cr_data_se_taste_cv,
                     complete_case = F,
                     location_name1 = "outputs/taste/taste_se_auc_catcont_10cv_hyper.csv")

func_randomforest_10cv_hyper(input_data = cr_data_full_taste_cv,
                     complete_case = F,
                     location_name1 = "outputs/taste/taste_full_auc_catcont_10cv_hyper.csv")

# 2. Validate
func_randomforest_10cv_hyper(input_data = cr_data_clin_validate_cv,  
                     complete_case = F,
                     location_name1 = "outputs/validate/validate_clin_auc_catcont_10cv_hyper.csv")

func_randomforest_10cv_hyper(input_data = cr_data_se_validate_cv,
                     complete_case = F,
                     location_name1 = "outputs/validate/validate_se_auc_catcont_10cv_hyper.csv")

func_randomforest_10cv_hyper(input_data = cr_data_full_validate_cv,  
                     complete_case = F,
                     location_name1 = "outputs/validate/validate_full_auc_catcont_10cv_hyper.csv")

#-------------------------------------------------------------------------------

# 1. Taste
func_randomforest_10cv_hyper(input_data = cr_data_clin_taste_cv,  
                             complete_case = T,
                             location_name1 = "outputs/taste/taste_clin_auc_completecase_10cv_hyper.csv")

func_randomforest_10cv_hyper(input_data = cr_data_se_taste_cv,
                             complete_case = T,
                             location_name1 = "outputs/taste/taste_se_auc_completecase_10cv_hyper.csv")

func_randomforest_10cv_hyper(input_data = cr_data_full_taste_cv,
                             complete_case = T,
                             location_name1 = "outputs/taste/taste_full_auc_completecase_10cv_hyper.csv")

# 2. Validate
func_randomforest_10cv_hyper(input_data = cr_data_clin_validate_cv,  
                             complete_case = T,
                             location_name1 = "outputs/validate/validate_clin_auc_completecase_10cv_hyper.csv")

func_randomforest_10cv_hyper(input_data = cr_data_se_validate_cv,
                             complete_case = T,
                             location_name1 = "outputs/validate/validate_se_auc_completecase_10cv_hyper.csv")

func_randomforest_10cv_hyper(input_data = cr_data_full_validate_cv,  
                             complete_case = T,
                             location_name1 = "outputs/validate/validate_full_auc_completecase_10cv_hyper.csv")

