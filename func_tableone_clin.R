# Clinical variables for table, to ensure to show missing by enrolled status
func_tableone_clin <- function(input_data, nonnormalvar, location_name, txt_name, taste) {
  
input_data <- {input_data} %>% 
    select(-(lopnr)) %>% 
    mutate(
    bmi_tertile = case_when(
      is.na(bmi) ~ "NA",
      bmi <= quantile(bmi, 1/3, na.rm = TRUE) ~ "1",
      bmi <= quantile(bmi, 2/3, na.rm = TRUE) ~ "2",
      TRUE ~ "3") ) %>%  #categorise so can see bmi missing values per group
    relocate(bmi_tertile, .after = bmi)

{ if (taste) { # two more continuous variables in taste to categorise to missing missing

input_data <- input_data %>% 
  mutate(
    ecgpcitm_tertile = case_when(
      is.na(ecgpcitm) ~ "NA",
      ecgpcitm <= quantile(ecgpcitm, 1/3, na.rm = TRUE) ~ "1",
      ecgpcitm <= quantile(ecgpcitm, 2/3, na.rm = TRUE) ~ "2",
      TRUE ~ "3") ) %>%  #categorise so can see ecgpcitm missing values per group
  relocate(ecgpcitm_tertile, .after = ecgpcitm) %>% 
  mutate(
    symppcitm_tertile = case_when(
      is.na(symppcitm) ~ "NA",
      symppcitm <= quantile(symppcitm, 1/3, na.rm = TRUE) ~ "1",
      symppcitm <= quantile(symppcitm, 2/3, na.rm = TRUE) ~ "2",
      TRUE ~ "3") ) %>%  #categorise so can see symppcitm missing values per group
  relocate(symppcitm_tertile, .after = symppcitm) 

} else {}}
  

#assign remaining column names to names (and remove if enrolled or not)
vars <- names(input_data)

#identify categorical variables
vars_cat <- names(Filter(is.factor, input_data)) 

#identify non normal variables
non_normal <- {nonnormalvar}

# Label categorical variables where needed
input_data$enrol <- factor(input_data$enrol, levels=c(2,1),
                             labels=c("Enrolled", "Not enrolled"))

# relabel missing variables "9" to missing

for (col in names(input_data)) {
  if (is.factor(input_data[[col]])) {
    # Convert "9" to NA
    input_data[[col]] <- as.character(input_data[[col]])
    input_data[[col]][input_data[[col]] == "9"] <- NA
    input_data[[col]] <- as.factor(input_data[[col]])
  }
}

tableone_temp <- CreateTableOne(vars = vars,
                                  factorVars = vars_cat,
                                  strata = "enrol",
                                  includeNA = TRUE,
                                  data = input_data)
  
tableoneprint <- print(tableone_temp,
                         nonnormal = non_normal,
                         contDigits = 1,
                         test = FALSE,
                         missing = TRUE,
                         quote = FALSE,
                         varLabel = FALSE,
                         dropEqual = TRUE,
                         noSpace = TRUE,
                         smd=TRUE)
  
# Save to output file
write.table(tableoneprint, here(paste0(location_name), paste0(txt_name)), sep="\t", quote = FALSE, row.names=TRUE, col.names=NA)
  
}


