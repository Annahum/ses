# Missing Data Functions
func_missing_factor <- function(x) {
  if (any(is.na(x))) {
    levels(x) <- c(levels(x), "9")
    x[is.na(x)] <- "9"
  }
  return(x)
}

func_categorize_continuous <- function(x) {
  if (!any(is.na(x))) {
    return(x)
  } else {
    return(ifelse(is.na(x), "5",
                  as.character(cut(x,
                                   breaks = quantile(x, probs = seq(0, 1, 0.25), na.rm = TRUE),
                                   include.lowest = TRUE))))
  }
}

#-------------------------------------------------------------------------------

func_randomforest_10cv_hyper <- function(input_data, location_name1,
                                             complete_case = FALSE,
                                             n_bootstrap = 1000,
                                             n_cores = parallel::detectCores() - 1) {
  
  set.seed(1)
  plan(multisession, workers = n_cores)
  
  base_folder <- dirname(location_name1)
  bootstrap_folder <- file.path(base_folder, "bootstraps")
  if (!dir.exists(bootstrap_folder)) dir.create(bootstrap_folder)
  
# -------------------- Data Preprocessing --------------------
  func_preprocess_data <- function(data) {
    data <- data %>%
      mutate(enrol = factor(enrol, levels = c(1, 2), labels = c("negative", "positive")))
    
    if (complete_case) {
      data <- data %>%
        mutate(across(where(is.factor), func_missing_factor)) %>%
        na.omit()
      
      factor_cols <- sapply(data, is.factor)
      data <- data[!apply(data[, factor_cols], 1, function(row) any(row == "9")), ]
    } else {
      data <- data %>%
        mutate(across(where(is.factor), func_missing_factor)) %>%
        mutate(across(where(is.numeric), func_categorize_continuous))
    }
    
    return(data)
  }
  
  data_rf_main <- func_preprocess_data(input_data) %>% select(-lopnr)
  
  # -------------------- Hyperparameter Grid --------------------
  hyper_grid <- expand.grid(
    mtry = c(2, floor(sqrt(ncol(data_rf_main)-1)), floor((ncol(data_rf_main)-1)/2)),
    min.node.size = c(1, 5, 10),
    num.trees = c(100, 250, 500),
    KEEP.OUT.ATTRS = FALSE
  )
  
  # -------------------- Bootstrap Setup --------------------
  pb <- progress_bar$new(
    format = "Bootstrap Progress [:bar] :percent eta: :eta",
    total = n_bootstrap, clear = FALSE, width = 60)
  
  bootstrap_file <- file.path(bootstrap_folder, 
                              paste0(tools::file_path_sans_ext(basename(location_name1)), "_individual_aucs.csv"))
  
  write.csv(data.frame(Bootstrap_Iteration = integer(), AUC = numeric()),
            bootstrap_file, row.names = FALSE)
  
  # -------------------- Bootstrap Function --------------------
  func_rf_bootstrap <- function(data, iteration) {
    pb$tick()
    
    boot_sample <- data[sample(1:nrow(data), replace = TRUE), ]
    data_rf <- func_preprocess_data(boot_sample)
    
    folds <- sample(rep(1:10, length.out = nrow(data_rf)))
    actual <- data_rf$enrol
    predicted_probs <- numeric(nrow(data_rf))
    
    for (k in 1:10) {
      test_data <- data_rf[folds == k, ]
      train_data <- data_rf[!data_rf$lopnr %in% test_data$lopnr, ]
      
      train_data <- train_data %>% select(-lopnr)
      test_data  <- test_data  %>% select(-lopnr)
      
      # Split train into train/validation for tuning
      idx <- sample(1:nrow(train_data), size = floor(0.8 * nrow(train_data)))
      train_split <- train_data[idx, ]
      val_split   <- train_data[-idx, ]
      
      auc_vals <- c()
      for (i in 1:nrow(hyper_grid)) {
        hp <- hyper_grid[i, ]
        
        model_hp <- ranger(enrol ~ ., 
                           data = train_split, 
                           probability = TRUE,
                           mtry = hp$mtry,
                           min.node.size = hp$min.node.size,
                           num.trees = hp$num.trees)
        
        probs_val <- predict(model_hp, data = val_split)$predictions[, "positive"]
        roc_val <- roc(val_split$enrol, probs_val, quiet = TRUE)
        auc_vals[i] <- auc(roc_val)
      }
      
      best_hp <- hyper_grid[which.max(auc_vals), ]
      
      final_model <- ranger(enrol ~ ., 
                            data = train_data, 
                            probability = TRUE,
                            mtry = best_hp$mtry,
                            min.node.size = best_hp$min.node.size,
                            num.trees = best_hp$num.trees)
      
      probs_test <- predict(final_model, data = test_data)$predictions[, "positive"]
      predicted_probs[folds == k] <- probs_test
    }
    
    roc_obj <- roc(actual, predicted_probs, levels = c("negative", "positive"))
    auc_val <- auc(roc_obj)
    
    write.table(data.frame(Bootstrap_Iteration = iteration, AUC = auc_val),
                bootstrap_file, append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
    
    return(auc_val)
  }
  
  # -------------------- Run Bootstraps --------------------
  auc_list <- future_map(1:n_bootstrap, ~ func_rf_bootstrap(input_data, .x), 
                         .options = furrr_options(seed = TRUE))
  
  valid_aucs <- unlist(auc_list[!is.na(auc_list)])
  auc_mean <- mean(valid_aucs)
  auc_ci <- quantile(valid_aucs, probs = c(0.025, 0.50, 0.975))
  
  # -------------------- Save Results --------------------
  results <- data.frame(
    AUC_Mean = auc_mean,
    CI_Lower = auc_ci[1],
    AUC_Median = auc_ci[2],
    CI_Upper = auc_ci[3]
  )
  
  write.csv(results, location_name1, row.names = FALSE)
  plan(sequential)
}
