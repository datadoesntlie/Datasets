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
    # Center the year variable to reduce multicollinearity
    Year_centered = Year - mean(Year, na.rm = TRUE),
    Year_squared = Year_centered^2,
    # Create lagged variables for robustness
    FLFP_lag2 = lag(FLFP, 2),
    CPR_lag3 = lag(CPR, 3),
    TFR_lag5 = lag(TFR, 5)
  ) %>%
  ungroup()

pdata <- pdata.frame(data_clean, index = c("Country", "Year"))

cat("=== ROBUSTNESS CHECKS FOR LEVELS METHODOLOGY ===\n\n")
cat("Testing levels approach with linear and quadratic time trends\n")
cat("Model Selection Hierarchy: Pooled > Random Effects > Fixed Effects\n\n")

dependent_vars <- c("Pension_GDP", "Old_age_dependency", 
                   "Pension_financing_gap", "Social_security_GDP")

robustness_results_levels <- list()

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

cat("=== ROBUSTNESS CHECK 1: LAGGED VARIABLES MODEL ===\n")
cat("Testing with FLFP lag 2, CPR lag 3, TFR lag 5\n\n")

lagged_results_levels <- list()

for (dep_var in dependent_vars) {
  
  cat("\n", rep("=", 60), "\n")
  cat("LAGGED MODEL FOR:", dep_var, "\n")
  cat(rep("=", 60), "\n")
  
  lagged_data <- pdata[!is.na(pdata[[dep_var]]) & 
                      !is.na(pdata$FLFP_lag2) & 
                      !is.na(pdata$CPR_lag3) & 
                      !is.na(pdata$TFR_lag5) &
                      !is.na(pdata$GDP_per_capita) &
                      !is.na(pdata$Life_expectancy_65) &
                      !is.na(pdata$Female_tertiary_education) &
                      !is.na(pdata$Urban_rate), ]
  
  if(nrow(lagged_data) < 30) {
    cat("Insufficient observations for lagged model of", dep_var, "\n")
    next
  }
  
  formula_lagged <- as.formula(paste(dep_var, "~ FLFP_lag2 + CPR_lag3 + TFR_lag5 + GDP_per_capita + Life_expectancy_65 + Female_tertiary_education + Urban_rate + Year_centered + Year_squared"))
  
  cat("Sample size:", nrow(lagged_data), "observations\n")
  
  # Estimate all three model types
  pooled_lagged <- plm(formula_lagged, data = lagged_data, model = "pooling")
  fe_lagged <- plm(formula_lagged, data = lagged_data, model = "within")
  re_lagged <- try(plm(formula_lagged, data = lagged_data, model = "random"), silent = TRUE)
  
  # Model selection
  if(!inherits(re_lagged, "try-error")) {
    best_lagged <- select_best_model_robust(pooled_lagged, fe_lagged, re_lagged, paste("Lagged", dep_var))
    lagged_model <- best_lagged$model
    lagged_type <- best_lagged$type
  } else {
    cat("Random Effects failed - using Fixed Effects\n")
    lagged_model <- fe_lagged
    lagged_type <- "Fixed Effects"
  }
  
  cat("Selected model type:", lagged_type, "\n")
  print(summary(lagged_model))
  
  lagged_results_levels[[dep_var]] <- list(
    model = lagged_model,
    type = lagged_type,
    data_size = nrow(lagged_data)
  )
  
  cat("\n", rep("-", 80), "\n")
}

cat("\n=== ROBUSTNESS CHECK 2: LEVELS vs FIRST DIFFERENCES CONSISTENCY ===\n")
cat("Comparing coefficient signs and significance across both methodological approaches\n\n")

# Load First Differences results for comparison
if(file.exists("econometric_models_fd_results.RData")) {
  load("econometric_models_fd_results.RData")
  fd_results <- models_results
} else {
  cat("First Differences results not found - loading from alternative location\n")
  fd_results <- NULL
}

# Load Levels results
if(file.exists("R/econometric_models_levels_results.RData")) {
  load("R/econometric_models_levels_results.RData")
  levels_results <- models_results_levels
} else {
  cat("Levels results not found\n")
  levels_results <- NULL
}

consistency_results <- list()

if(!is.null(fd_results) && !is.null(levels_results)) {
  
  cat("=== COEFFICIENT CONSISTENCY ANALYSIS ===\n")
  
  # Create mapping between dependent variables
  fd_dep_vars <- c("d_Pension_GDP", "d_Old_age_dependency", "d_Pension_financing_gap", "d_Social_security_GDP")
  levels_dep_vars <- c("Pension_GDP", "Old_age_dependency", "Pension_financing_gap", "Social_security_GDP")
  
  key_variables <- c("FLFP", "CPR", "TFR")  # Focus on key policy variables
  
  # Create comprehensive comparison table
  comparison_table <- data.frame(
    Variable = character(),
    Model = character(),
    Levels_Coef = numeric(),
    Levels_SE = numeric(),
    Levels_Sig = character(),
    FD_Coef = numeric(),
    FD_SE = numeric(),
    FD_Sig = character(),
    Sign_Consistent = character(),
    Magnitude_Ratio = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(i in 1:length(fd_dep_vars)) {
    
    fd_dep <- fd_dep_vars[i]
    levels_dep <- levels_dep_vars[i]
    model_name <- gsub("d_|_", " ", fd_dep)
    model_name <- tools::toTitleCase(model_name)
    
    cat("\n", rep("=", 60), "\n")
    cat("CONSISTENCY CHECK FOR:", model_name, "\n")
    cat(rep("=", 60), "\n")
    
    # Get models
    fd_model_key <- paste0("model", i, "_best")
    levels_model_key <- paste0("model", i, "_best")
    
    if(fd_model_key %in% names(fd_results) && levels_model_key %in% names(levels_results)) {
      
      fd_model <- fd_results[[fd_model_key]]
      levels_model <- levels_results[[levels_model_key]]
      
      fd_coefs <- coef(fd_model)
      levels_coefs <- coef(levels_model)
      
      fd_summary <- summary(fd_model)
      levels_summary <- summary(levels_model)
      
      cat("Levels Model Type:", levels_results[[paste0("model", i, "_type")]], "\n")
      cat("FD Model Type:", fd_results[[paste0("model", i, "_type")]], "\n\n")
      
      cat("Variable\t\tLevels\t\t\tFirst Differences\t\tConsistency\n")
      cat(rep("-", 80), "\n")
      
      for(var in key_variables) {
        
        # Map variable names
        levels_var <- var
        fd_var <- paste0("d_", var)
        
        if(levels_var %in% names(levels_coefs) && fd_var %in% names(fd_coefs)) {
          
          levels_coef <- levels_coefs[levels_var]
          fd_coef <- fd_coefs[fd_var]
          
          # Get standard errors and p-values
          levels_se <- levels_summary$coefficients[levels_var, "Std. Error"]
          fd_se <- fd_summary$coefficients[fd_var, "Std. Error"]
          
          levels_pval <- levels_summary$coefficients[levels_var, "Pr(>|t|)"]
          fd_pval <- fd_summary$coefficients[fd_var, "Pr(>|t|)"]
          
          # Determine significance
          get_sig <- function(p) {
            if(p < 0.01) return("***")
            else if(p < 0.05) return("**")
            else if(p < 0.1) return("*")
            else return("")
          }
          
          levels_sig <- get_sig(levels_pval)
          fd_sig <- get_sig(fd_pval)
          
          # Check sign consistency
          same_sign <- sign(levels_coef) == sign(fd_coef)
          sign_consistency <- ifelse(same_sign, "Consistent", "Inconsistent")
          
          # Magnitude comparison (ratio of FD to Levels)
          mag_ratio <- abs(fd_coef) / abs(levels_coef)
          
          cat(sprintf("%-15s\t%.4f%s (%.4f)\t%.4f%s (%.4f)\t\t%s\n", 
                     var, levels_coef, levels_sig, levels_se, 
                     fd_coef, fd_sig, fd_se, sign_consistency))
          
          # Store in comparison table
          comparison_table <- rbind(comparison_table, data.frame(
            Variable = var,
            Model = model_name,
            Levels_Coef = levels_coef,
            Levels_SE = levels_se,
            Levels_Sig = levels_sig,
            FD_Coef = fd_coef,
            FD_SE = fd_se,
            FD_Sig = fd_sig,
            Sign_Consistent = sign_consistency,
            Magnitude_Ratio = mag_ratio
          ))
        }
      }
    }
  }
  
  # Save comparison results
  consistency_results$comparison_table <- comparison_table
  write.csv(comparison_table, "R/levels_fd_consistency_comparison.csv", row.names = FALSE)
  
  cat("\n", rep("=", 80), "\n")
  cat("=== OVERALL CONSISTENCY SUMMARY ===\n")
  
  # Calculate overall consistency statistics
  consistent_signs <- sum(comparison_table$Sign_Consistent == "Consistent")
  total_comparisons <- nrow(comparison_table)
  consistency_rate <- consistent_signs / total_comparisons * 100
  
  cat("Total variable-model comparisons:", total_comparisons, "\n")
  cat("Consistent signs:", consistent_signs, "\n")
  cat("Sign consistency rate:", round(consistency_rate, 1), "%\n")
  
  # Significance consistency
  both_sig <- sum(comparison_table$Levels_Sig != "" & comparison_table$FD_Sig != "")
  levels_only_sig <- sum(comparison_table$Levels_Sig != "" & comparison_table$FD_Sig == "")
  fd_only_sig <- sum(comparison_table$Levels_Sig == "" & comparison_table$FD_Sig != "")
  neither_sig <- sum(comparison_table$Levels_Sig == "" & comparison_table$FD_Sig == "")
  
  cat("\nSignificance patterns:\n")
  cat("- Both significant:", both_sig, "\n")
  cat("- Levels only significant:", levels_only_sig, "\n")
  cat("- FD only significant:", fd_only_sig, "\n")
  cat("- Neither significant:", neither_sig, "\n")
  
} else {
  cat("Cannot perform consistency analysis - missing results files\n")
}

# Store all robustness results
robustness_results_levels$lagged <- lagged_results_levels
robustness_results_levels$consistency <- consistency_results

save(robustness_results_levels, file = "R/robustness_results_levels.RData")

cat("\n=== CREATING ROBUSTNESS COMPARISON TABLES ===\n")

# Load main results for comparison
if(file.exists("R/econometric_models_levels_results.RData")) {
  load("R/econometric_models_levels_results.RData")
  main_results <- models_results_levels
} else {
  cat("Main results not found - cannot create comparison\n")
  main_results <- NULL
}

# Create comparison tables if main results exist
if(!is.null(main_results)) {
  
  # Collect models for stargazer comparison
  comparison_models <- list()
  model_labels <- c()
  
  for(dep_var in dependent_vars) {
    
    dep_var_clean <- gsub("_", " ", dep_var)
    dep_var_clean <- tools::toTitleCase(dep_var_clean)
    
    # Main model
    if(paste0("model", which(dependent_vars == dep_var), "_best") %in% names(main_results)) {
      main_model_key <- paste0("model", which(dependent_vars == dep_var), "_best")
      comparison_models[[paste0(dep_var_clean, " (Main)")]] <- main_results[[main_model_key]]
      model_labels <- c(model_labels, paste0(dep_var_clean, " (Main)"))
    }
    
    # Lagged model
    if(dep_var %in% names(lagged_results_levels)) {
      comparison_models[[paste0(dep_var_clean, " (Lagged)")]] <- lagged_results_levels[[dep_var]]$model
      model_labels <- c(model_labels, paste0(dep_var_clean, " (Lagged)"))
    }
    
    # Consistency analysis results (not included in stargazer as they're different models)
    # but we can reference them in the documentation
  }
  
  # Create comprehensive robustness table
  if(length(comparison_models) > 0) {
    stargazer(comparison_models,
              type = "text",
              title = "Robustness Checks: Levels Methodology with Lagged Variables",
              column.labels = model_labels,
              covariate.labels = c("Female Labor Participation (t-2)",
                                 "Contraceptive Prevalence Rate (t-3)",
                                 "Total Fertility Rate (t-5)",
                                 "Female Labor Participation",
                                 "Contraceptive Prevalence Rate",
                                 "Total Fertility Rate",
                                 "GDP per capita",
                                 "Life Expectancy at 65",
                                 "Female Tertiary Education",
                                 "Urban Rate",
                                 "Year Centered",
                                 "Year Squared"),
              out = "R/robustness_levels_results_table.txt")
    
    cat("Robustness comparison table saved to: R/robustness_levels_results_table.txt\n")
  }
}

cat("\n=== ROBUSTNESS SUMMARY FOR LEVELS METHODOLOGY ===\n")
cat("1. Lagged Variables Model: Tests delayed effects of key variables\n")
cat("2. Levels vs First Differences Consistency: Cross-methodological validation\n")
cat("3. Model selection hierarchy applied consistently across all robustness checks\n")
cat("4. Results saved to: robustness_results_levels.RData\n")

cat("\n=== METHODOLOGICAL COMPARISON INSIGHTS ===\n")
cat("- Levels approach: Captures long-term equilibrium relationships\n")
cat("- First Differences approach: Captures short-term adjustment dynamics\n")
cat("- Consistent signs across both approaches strengthen inference\n")
cat("- Divergent results may indicate different time horizons of effects\n")
cat("- Significance patterns reveal temporal nature of relationships\n")

cat("\n=== OUTPUT FILES GENERATED ===\n")
cat("- R/robustness_levels_results_table.txt: Lagged variables comparison\n")
cat("- R/levels_fd_consistency_comparison.csv: Detailed coefficient comparison\n")
cat("- R/robustness_results_levels.RData: All robustness results\n")

cat("Robustness analysis complete for levels methodology\n")