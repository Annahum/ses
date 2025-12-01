
# R-PROFILE --------------------------------------------------------------------
source("W:/C6_Berglund/ahumphreys/r-profile.R")

# PACKAGES ---------------------------------------------------------------------

library(here)
library(dplyr)
library(ggplot2)
library(tableone)
library(tidyr)
library(haven)
library(clustMixType)
library(stringr)
library(patchwork)
library(berryFunctions)
library(ggtext)
library(lubridate)
library(randomForest)
library(caret)
library(pROC)
library(purrr)
library(progress)
library(ranger)
library(furrr)
library(future)
library(progressr)
library(pROC)
library(caret)


# GLOBAL FUNCTIONS -------------------------------------------------------------

# function to fully format variables and make one row per participant
source(here("scripts", "func_formatting.R"))

# function to make full population
source(here("scripts", "func_fullpop.R"))

# function to format table one
source(here("scripts", "func_tableone.R"))

# function to format table one - clinical
source(here("scripts", "func_tableone_clin.R"))

# function to create predictive model using random forest and assess model fit - 10 fold CV
source(here("scripts", "func_randomforest_cv_10fold_hyper.R"))

# DATA PREPARATION -------------------------------------------------------------

# Identify scripts to run
run_01_cr_baseloading_taste <- 0
run_01_cr_baseloading_validate <- 0
run_02_cr_inclusion_taste <- 0
run_02_cr_inclusion_validate <- 0
run_03_cr_covariate <- 0
run_04_cr_full_pop <- 0
run_04_cr_full_pop_load_taste <- 1
run_04_cr_full_pop_load_taste_extra <- 0
run_04_cr_full_pop_load_validate <- 1
run_04_cr_full_pop_load_validate_extra <- 0
run_05_cr_clin_pop <- 1
run_05_cr_rf_pop <- 1

# Base formatting and importing of datasets for taste and validate
if (run_01_cr_baseloading_taste) source(here("scripts", "01_cr_base_loading_taste.R"))
if (run_01_cr_baseloading_validate) source(here("scripts", "01_cr_base_loading_validate.R"))

# Prepare base populations from trial data 
if (run_02_cr_inclusion_taste) source(here("scripts", "02_cr_inclusion_taste.R"))
if (run_02_cr_inclusion_validate) source(here("scripts", "02_cr_inclusion_validate.R"))

# Further formating of covariates to determine baseline values using one function for taste and validate
if (run_03_cr_covariate) source(here("scripts", "03_cr_covariate.R"))

# Make full pop using one function for taste and validate, including subgroups.
if (run_04_cr_full_pop) source(here("scripts", "04_cr_full_pop.R"))
if (run_04_cr_full_pop_load_taste) load("W:/C6_Berglund/data/procdata/ses_trial_characteristics/taste/cr_data_se_taste.Rda")
if (run_04_cr_full_pop_load_taste_extra) load("W:/C6_Berglund/data/procdata/ses_trial_characteristics/taste/cr_data_se_taste_younger.Rda")
if (run_04_cr_full_pop_load_taste_extra) load("W:/C6_Berglund/data/procdata/ses_trial_characteristics/taste/cr_data_se_taste_older.Rda")
if (run_04_cr_full_pop_load_taste_extra) load("W:/C6_Berglund/data/procdata/ses_trial_characteristics/taste/cr_data_se_taste_male.Rda")
if (run_04_cr_full_pop_load_taste_extra) load("W:/C6_Berglund/data/procdata/ses_trial_characteristics/taste/cr_data_se_taste_female.Rda")

if (run_04_cr_full_pop_load_validate) load("W:/C6_Berglund/data/procdata/ses_trial_characteristics/validate/cr_data_se_validate.Rda")
if (run_04_cr_full_pop_load_validate_extra) load("W:/C6_Berglund/data/procdata/ses_trial_characteristics/validate/cr_data_se_validate_younger.Rda")
if (run_04_cr_full_pop_load_validate_extra) load("W:/C6_Berglund/data/procdata/ses_trial_characteristics/validate/cr_data_se_validate_older.Rda")
if (run_04_cr_full_pop_load_validate_extra) load("W:/C6_Berglund/data/procdata/ses_trial_characteristics/validate/cr_data_se_validate_male.Rda")
if (run_04_cr_full_pop_load_validate_extra) load("W:/C6_Berglund/data/procdata/ses_trial_characteristics/validate/cr_data_se_validate_female.Rda")

# Make base population from table 1 in the trials
if (run_05_cr_clin_pop) source(here("scripts", "05_cr_full_clin_pop.R"))
if (run_04_cr_full_pop_load_validate) load("W:/C6_Berglund/data/procdata/ses_trial_characteristics/taste/cr_data_clin_taste.Rda")
if (run_04_cr_full_pop_load_taste_extra) load("W:/C6_Berglund/data/procdata/ses_trial_characteristics/validate/cr_data_clin_validate.Rda")

# Make final populations for random forest
if (run_05_cr_rf_pop) source(here("scripts", "05_cr_full_rf_pop.R"))

# DATA ANALYSIS ----------------------------------------------------------------
run_02_analysis <- 0

if (run_02_analysis) source(here("scripts", "02_analysis.R"))

# ------------------------------------------------------------------------------


