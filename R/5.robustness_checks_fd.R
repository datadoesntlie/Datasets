library(plm)
library(lmtest)
library(sandwich)
library(stargazer)
library(dplyr)
library(readr)

data <- read_csv("R/master_dataset.csv")

data_clean <- data %>%
  filter(!is.na(Country) & !is.na(Year)) %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%
  mutate(
    # Create first differences for all variables
    d_Pension_GDP = Pension_GDP - lag(Pension_GDP),
    d_Old_age_dependency = Old_age_dependency - lag(Old_age_dependency),
    d_Pension_financing_gap = Pension_financing_gap - lag(Pension_financing_gap),
    d_Social_security_GDP = Social_security_GDP - lag(Social_security_GDP),
    d_FLFP = FLFP - lag(FLFP),
    d_CPR = CPR - lag(CPR),
    d_TFR = TFR - lag(TFR),
    d_GDP_per_capita = GDP_per_capita - lag(GDP_per_capita),
    d_Life_expectancy_65 = Life_expectancy_65 - lag(Life_expectancy_65),
    d_Female_tertiary_education = Female_tertiary_education - lag(Female_tertiary_education),
    d_Urban_rate = Urban_rate - lag(Urban_rate),
    # Create lagged first differences for robustness
    d_FLFP_lag2 = lag(d_FLFP, 2),
    d_CPR_lag3 = lag(d_CPR, 3),
    d_TFR_lag5 = lag(d_TFR, 5)
  ) %>%
  ungroup()

pdata <- pdata.frame(data_clean, index = c("Country", "Year"))

cat("=== ROBUSTNESS CHECKS FOR FIRST DIFFERENCES METHODOLOGY ===\n\n")
cat("Testing full first differences approach (all variables differenced)\n")
cat("Model Selection Hierarchy: Pooled > Random Effects > Fixed Effects\n\n")

dependent_vars <- c("d_Pension_GDP", "d_Old_age_dependency", 
                   "d_Pension_financing_gap", "d_Social_security_GDP")

robustness_results_fd <- list()

# Function to select best model based on hierarchy
select_best_model_robust <- function(pooled_model, fe_model, re_model, model_name) {
  
  # F-test: Pooled vs Fixed Effects
  f_test <- pFtest(fe_model, pooled_model)
  
  # Hausman test: Fixed Effects vs Random Effects (with error handling)
  hausman_test <- try(phtest(fe_model, re_model), silent = TRUE)
  if(inherits(hausman_test, "try-error")) {
    hausman_test <- list(p.value = 0)  # Force FE selection if Hausman test fails
  }
  
  # Decision logic: Preference order is Pooled > Random Effects > Fixed Effects
  if (f_test$p.value > 0.05) {
    selected_model <- pooled_model
    selected_type <- "Pooled"
  } else {
    if (hausman_test$p.value > 0.05) {
      selected_model <- re_model
      selected_type <- "Random Effects"
    } else {
      selected_model <- fe_model
      selected_type <- "Fixed Effects"
    }
  }
  
  return(list(model = selected_model, type = selected_type, 
              f_test = f_test, hausman_test = hausman_test))
}

cat("=== ROBUSTNESS CHECK 1: LAGGED FIRST DIFFERENCES MODEL ===\n")
cat("Testing with lagged first differences: FLFP lag 2, CPR lag 3, TFR lag 5\n\n")

lagged_results_fd <- list()

for (dep_var in dependent_vars) {
  
  cat("\n", rep("=", 60), "\n")
  cat("LAGGED FIRST DIFFERENCES MODEL FOR:", dep_var, "\n")
  cat(rep("=", 60), "\n")
  
  lagged_data <- pdata[!is.na(pdata[[dep_var]]) & 
                      !is.na(pdata$d_FLFP_lag2) & 
                      !is.na(pdata$d_CPR_lag3) & 
                      !is.na(pdata$d_TFR_lag5) &
                      !is.na(pdata$d_GDP_per_capita) &
                      !is.na(pdata$d_Life_expectancy_65) &
                      !is.na(pdata$d_Female_tertiary_education) &
                      !is.na(pdata$d_Urban_rate), ]
  
  if(nrow(lagged_data) < 30) {
    cat("Insufficient observations for lagged first differences model of", dep_var, "\n")
    next
  }
  
  formula_lagged <- as.formula(paste(dep_var, "~ d_FLFP_lag2 + d_CPR_lag3 + d_TFR_lag5 + d_GDP_per_capita + d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate"))
  
  cat("Sample size:", nrow(lagged_data), "observations\n")
  
  # Estimate all three model types
  pooled_lagged <- plm(formula_lagged, data = lagged_data, model = "pooling")
  fe_lagged <- plm(formula_lagged, data = lagged_data, model = "within")
  re_lagged <- try(plm(formula_lagged, data = lagged_data, model = "random"), silent = TRUE)
  
  # Model selection
  if(!inherits(re_lagged, "try-error")) {
    best_lagged <- select_best_model_robust(pooled_lagged, fe_lagged, re_lagged, paste("Lagged FD", dep_var))
    lagged_model <- best_lagged$model
    lagged_type <- best_lagged$type
  } else {
    cat("Random Effects failed - using Fixed Effects\n")
    lagged_model <- fe_lagged
    lagged_type <- "Fixed Effects"
  }
  
  cat("Selected model type:", lagged_type, "\n")
  print(summary(lagged_model))
  
  lagged_results_fd[[dep_var]] <- list(
    model = lagged_model,
    type = lagged_type,
    data_size = nrow(lagged_data)
  )
  
  cat("\n", rep("-", 80), "\n")
}

cat("\n=== ROBUSTNESS CHECK 2: CROSS-CHECK ACROSS DIFFERENT ESTIMATORS ===\n")
cat("Testing consistency across Pooled OLS, Fixed Effects, and Random Effects\n\n")

estimator_results_fd <- list()

for (dep_var in dependent_vars) {
  
  cat("\n", rep("=", 60), "\n")
  cat("ESTIMATOR CROSS-CHECK FOR:", dep_var, "\n")
  cat(rep("=", 60), "\n")
  
  # Use same data as main models
  estimator_data <- pdata[!is.na(pdata[[dep_var]]) & 
                         !is.na(pdata$d_FLFP) & 
                         !is.na(pdata$d_CPR) & 
                         !is.na(pdata$d_TFR) &
                         !is.na(pdata$d_GDP_per_capita) &
                         !is.na(pdata$d_Life_expectancy_65) &
                         !is.na(pdata$d_Female_tertiary_education) &
                         !is.na(pdata$d_Urban_rate), ]
  
  if(nrow(estimator_data) < 30) {
    cat("Insufficient observations for estimator cross-check of", dep_var, "\n")
    next
  }
  
  formula_estimator <- as.formula(paste(dep_var, "~ d_FLFP + d_CPR + d_TFR + d_GDP_per_capita + d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate"))
  
  cat("Sample size:", nrow(estimator_data), "observations\n")
  
  # Estimate ALL three model types for comparison
  cat("\n--- Pooled OLS ---\n")
  pooled_est <- plm(formula_estimator, data = estimator_data, model = "pooling")
  print(summary(pooled_est))
  
  cat("\n--- Fixed Effects ---\n")
  fe_est <- plm(formula_estimator, data = estimator_data, model = "within")
  print(summary(fe_est))
  
  cat("\n--- Random Effects ---\n")
  re_est <- try(plm(formula_estimator, data = estimator_data, model = "random"), silent = TRUE)
  if(!inherits(re_est, "try-error")) {
    print(summary(re_est))
  } else {
    cat("Random Effects estimation failed\n")
    re_est <- NULL
  }
  
  # Store all estimators for comparison
  estimator_results_fd[[dep_var]] <- list(
    pooled = pooled_est,
    fixed_effects = fe_est,
    random_effects = re_est,
    data_size = nrow(estimator_data)
  )
  
  # Compare key coefficients across estimators
  cat("\n--- COEFFICIENT COMPARISON ---\n")
  
  # Extract key coefficients
  pooled_coefs <- coef(pooled_est)
  fe_coefs <- coef(fe_est)
  
  key_vars <- c("d_FLFP", "d_CPR", "d_TFR")
  
  cat("Variable\t\tPooled\t\tFixed Effects\t\tRandom Effects\n")
  cat(rep("-", 70), "\n")
  
  for(var in key_vars) {
    if(var %in% names(pooled_coefs) && var %in% names(fe_coefs)) {
      pooled_val <- round(pooled_coefs[var], 4)
      fe_val <- round(fe_coefs[var], 4)
      
      if(!is.null(re_est) && var %in% names(coef(re_est))) {
        re_val <- round(coef(re_est)[var], 4)
        cat(var, "\t\t", pooled_val, "\t\t", fe_val, "\t\t\t", re_val, "\n")
      } else {
        cat(var, "\t\t", pooled_val, "\t\t", fe_val, "\t\t\tN/A\n")
      }
    }
  }
  
  cat("\n", rep("-", 80), "\n")
}

# Store all robustness results
robustness_results_fd$lagged <- lagged_results_fd
robustness_results_fd$estimator_comparison <- estimator_results_fd

save(robustness_results_fd, file = "R/robustness_results_fd.RData")

cat("\n=== CREATING ROBUSTNESS COMPARISON TABLES ===\n")

# Load main results for comparison
if(file.exists("econometric_models_fd_results.RData")) {
  load("econometric_models_fd_results.RData")
  main_results <- models_results
} else {
  cat("Main FD results not found - cannot create comparison\n")
  main_results <- NULL
}

# Create comparison tables if main results exist
if(!is.null(main_results)) {
  
  # Collect models for stargazer comparison
  comparison_models <- list()
  model_labels <- c()
  
  for(dep_var in dependent_vars) {
    
    dep_var_clean <- gsub("d_", "", dep_var)
    dep_var_clean <- gsub("_", " ", dep_var_clean)
    dep_var_clean <- tools::toTitleCase(dep_var_clean)
    
    # Main model
    if(paste0("model", which(dependent_vars == dep_var), "_best") %in% names(main_results)) {
      main_model_key <- paste0("model", which(dependent_vars == dep_var), "_best")
      comparison_models[[paste0(dep_var_clean, " (Main)")]] <- main_results[[main_model_key]]
      model_labels <- c(model_labels, paste0(dep_var_clean, " (Main)"))
    }
    
    # Lagged model
    if(dep_var %in% names(lagged_results_fd)) {
      comparison_models[[paste0(dep_var_clean, " (Lagged)")]] <- lagged_results_fd[[dep_var]]$model
      model_labels <- c(model_labels, paste0(dep_var_clean, " (Lagged)"))
    }
    
    # Add all estimators for this dependent variable
    if(dep_var %in% names(estimator_results_fd)) {
      est_results <- estimator_results_fd[[dep_var]]
      
      # Pooled OLS
      comparison_models[[paste0(dep_var_clean, " (Pooled)")]] <- est_results$pooled
      model_labels <- c(model_labels, paste0(dep_var_clean, " (Pooled)"))
      
      # Fixed Effects
      comparison_models[[paste0(dep_var_clean, " (FE)")]] <- est_results$fixed_effects
      model_labels <- c(model_labels, paste0(dep_var_clean, " (FE)"))
      
      # Random Effects (if available)
      if(!is.null(est_results$random_effects)) {
        comparison_models[[paste0(dep_var_clean, " (RE)")]] <- est_results$random_effects
        model_labels <- c(model_labels, paste0(dep_var_clean, " (RE)"))
      }
    }
  }
  
  # Create comprehensive robustness table
  if(length(comparison_models) > 0) {
    stargazer(comparison_models,
              type = "text",
              title = "Robustness Checks: First Differences with Estimator Comparison",
              column.labels = model_labels,
              covariate.labels = c("Δ Female Labor Participation (t-2)",
                                 "Δ Contraceptive Prevalence Rate (t-3)",
                                 "Δ Total Fertility Rate (t-5)",
                                 "Δ Female Labor Participation",
                                 "Δ Contraceptive Prevalence Rate", 
                                 "Δ Total Fertility Rate",
                                 "Δ GDP per capita",
                                 "Δ Life Expectancy at 65",
                                 "Δ Female Tertiary Education",
                                 "Δ Urban Rate"),
              out = "R/robustness_fd_results_table.txt")
    
    cat("Robustness comparison table saved to: R/robustness_fd_results_table.txt\n")
  }
}

cat("\n=== ROBUSTNESS SUMMARY FOR FIRST DIFFERENCES METHODOLOGY ===\n")
cat("1. Lagged First Differences Model: Tests delayed adjustment effects\n")
cat("2. Estimator Cross-Check: Compares Pooled OLS, Fixed Effects, and Random Effects\n")
cat("3. Model selection hierarchy applied consistently across all robustness checks\n")
cat("4. Results saved to: robustness_results_fd.RData\n")

cat("\n=== ESTIMATOR COMPARISON INSIGHTS ===\n")
cat("- Pooled OLS: Assumes no unobserved heterogeneity\n")
cat("- Fixed Effects: Controls for time-invariant country characteristics\n")
cat("- Random Effects: Assumes orthogonality between country effects and regressors\n")
cat("- Coefficient stability across estimators indicates robustness\n")
cat("- Large differences may suggest model misspecification or heterogeneity\n")

cat("\n=== FIRST DIFFERENCES ADVANTAGES ===\n")
cat("- Removes unobserved country heterogeneity automatically\n")
cat("- Reduces non-stationarity concerns\n")
cat("- Focuses on short-term adjustment dynamics\n")
cat("- Less sensitive to outliers in levels\n")

cat("Robustness analysis complete for first differences methodology\n")