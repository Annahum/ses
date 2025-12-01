func_tableone <- function(input_data, location_name, txt_name, convert_na_to_factor=FALSE) {
  
  input_data <- input_data %>% 
    select(-lopnr)
  
  # Assign remaining column names to vars (excluding 'enrol')
  vars <- names(input_data)
  vars <- vars[!vars %in% c("enrol")]
  
  # Identify categorical variables
  vars_cat <- names(Filter(is.factor, input_data))  
  
  # Label categorical variables
  input_data$enrol <- factor(input_data$enrol, levels = c(2, 1),
                             labels = c("Enrolled", "Not enrolled"))
  
  input_data$gender <- factor(input_data$gender, levels = c(1, 2),
                              labels = c("Male", "Female"))
  
  input_data$birthcountry_cat <- factor(input_data$birthcountry_cat, levels = 1:5,
                                        labels = c("Sweden", "Other Nordic countries", "Europe excluding Nordics", "North and South America", "Other"))
  
  input_data$baseline_swedish_region <- factor(input_data$baseline_swedish_region, levels = 1:6,
                                               labels = c("Stockholm", "Uppsala", "Linkoping", "Lund-Malmo", "Gothenburg", "Umea"))
  
  input_data$baseline_degurba_code <- factor(input_data$baseline_degurba_code, levels = 1:3,
                                             labels = c("Most urbanised", "Mid urbanised", "Least urbanised"))
  
  input_data$baseline_family_overall <- factor(input_data$baseline_family_overall, levels = 1:4,
                                               labels = c("Single", "Single parent", "Married, cohabiting, in a partnership",
                                                          "People with incomplete/contradictory information"))
  
  input_data$baseline_yseg_cat <- factor(input_data$baseline_yseg_cat, levels = 1:6,
                                         labels = c("Senior Officials, managers and professionals", "Small business owners and farmers",
                                                    "Supervisors, technicians and skilled manual workers", 
                                                    "Office employees and skilled workers in trade, services, and care",
                                                    "Other manual worker",
                                                    "No occupational status or unemployed"))
  
  input_data$baseline_education_cat <- factor(input_data$baseline_education_cat, levels = 1:4,
                                              labels = c("General education", "Social sciences", "Science, technology, agriculture, health and social care", "Services"))
  
  input_data$baseline_education_length <- factor(input_data$baseline_education_length, levels = 1:3,
                                                 labels = c("Primary education <=9yrs", "Secondary education <=3yrs", "Tertiary education </>3yrs"))
  
  # Optional: Add "Missing" category to variables with NA 
  if (convert_na_to_factor) {
    input_data <- input_data %>%
      mutate(across(
        .fns = ~ {
          if (is.factor(.x)) {
            if (any(is.na(.x))) {
              .x <- addNA(.x)
              levels(.x)[is.na(levels(.x))] <- "Missing"
              .x[is.na(.x)] <- "Missing"
            }
            return(.x)
          } else if (is.numeric(.x)) {
            if (any(is.na(.x))) {
              bins <- cut(.x,
                          breaks = quantile(.x, probs = seq(0, 1, 0.25), na.rm = TRUE),
                          include.lowest = TRUE)
              bins_char <- as.character(bins)
              bins_char[is.na(.x)] <- "Missing"
              return(factor(bins_char))
            } else {
              return(.x)
            }
          } else {
            return(.x)
          }
        }
      ))
  }
  
  # Create TableOne
  tableone_temp <- CreateTableOne(vars = vars,
                                  factorVars = vars_cat,
                                  strata = "enrol",
                                  includeNA = FALSE,
                                  data = input_data)
  
  tableoneprint <- print(tableone_temp,
                         nonnormal = setdiff(vars, "age"),
                         contDigits = 1,
                         test = FALSE,
                         missing = TRUE,
                         quote = FALSE,
                         varLabel = FALSE,
                         dropEqual = TRUE,
                         noSpace = TRUE,
                         smd = TRUE)
  
  # Save to output file
  write.table(tableoneprint, here(paste0(location_name), paste0(txt_name)), sep = "\t", quote = FALSE, row.names = TRUE, col.names = NA)
  
  base <- read.delim(here(paste0(location_name), paste0(txt_name)), sep = "\t") %>%
    rename("Not enrolled" = "Not.enrolled") %>%
    relocate(Missing, .before = "SMD") %>%
    mutate(X = str_remove(X, " \\(%\\)")) %>%
    mutate(X = str_remove(X, " \\(median \\[IQR\\]\\)")) %>%
    mutate(X = case_when(
      X == "n" ~ "N",
      X == "age (mean (SD))" ~ "Age (mean (SD))",
      X == "gender" ~ "Female",
      X == "birthcountry_cat" ~ "Country of birth", 
      X == "migration_3ybefore" ~ "Immigration in the 3 years prior",
      X == "baseline_swedish_region" ~ "Region of residence",  
      X == "baseline_degurba_code" ~ "Degree of urbanisation", 
      X == "moved_year_before" ~ "Moved municipality in last year", 
      X == "separation_widow_3y" ~ "Divorce, separation, or loss of partner in last 3 years", 
      X == "baseline_family_overall" ~ "Family position", 
      X == "baseline_education_length" ~ "Education length", 
      X == "baseline_education_cat" ~ "Education area", 
      X == "baseline_yseg_cat" ~ "Occupational status", 
      X == "unemployed_3ybefore" ~ "At least one day of unemployment last 3 years",
      X == "income_disposable_3ymean" ~ "Individual disposable income per hundred Kroner",
      X == "income_disposable_3ymean_q5" ~ "Individual disposable income per hundred Kroner by quintile",
      TRUE ~ X))
  
  # Replace all NAs with ""
  base[is.na(base)] <- ""
  
  # Insert header rows only if convert_na_to_factor is FALSE
  if (!convert_na_to_factor) {
    demo <- c("Demographics and migration", "", "", "", "")
    civil <- c("Family structure", "", "", "", "")
    edu <- c("Education", "", "", "", "")
    employment <- c("Employment and income", "", "", "", "")
    
    base <- base %>%
      insertRows(2, demo) %>%
      insertRows(23, civil) %>%
      insertRows(29, edu) %>%
      insertRows(39, employment)
  }
  
  write.table(base, 
              here(paste0(location_name), paste0(txt_name)), 
              sep = "\t", 
              quote = FALSE, 
              row.names = FALSE)
}

