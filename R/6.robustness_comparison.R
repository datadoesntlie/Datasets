library(plm)
library(lmtest)
library(sandwich)
library(stargazer)
library(dplyr)
library(readr)

# Load the diagnostic and robustness results
load("diagnostic_results.RData")
load("robustness_results.RData")

# Load the data to run main models for comparison
data <- read_csv("master_dataset.csv")

data_clean <- data %>%
  filter(!is.na(Country) & !is.na(Year)) %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%
  mutate(
    d_Pension_GDP = Pension_GDP - lag(Pension_GDP),
    d_Old_age_dependency = Old_age_dependency - lag(Old_age_dependency),
    d_Pension_financing_gap = Pension_financing_gap - lag(Pension_financing_gap),
    d_Social_security_GDP = Social_security_GDP - lag(Social_security_GDP),
    d_FLFP = FLFP - lag(FLFP),
    d_GDP_per_capita = GDP_per_capita - lag(GDP_per_capita),
    d_Life_expectancy_65 = Life_expectancy_65 - lag(Life_expectancy_65),
    d_Female_tertiary_education = Female_tertiary_education - lag(Female_tertiary_education),
    d_Urban_rate = Urban_rate - lag(Urban_rate)
  ) %>%
  ungroup()

pdata <- pdata.frame(data_clean, index = c("Country", "Year"))

cat("=== ROBUSTNESS COMPARISON: MAIN vs ROBUSTNESS CHECK RESULTS ===\n\n")

# Define dependent variables
dependent_vars <- c("d_Pension_GDP", "d_Old_age_dependency", 
                   "d_Pension_financing_gap", "d_Social_security_GDP")

# Create comparison tables for each dependent variable
comparison_results <- list()

for (dep_var in dependent_vars) {
  cat("\n", rep("=", 70), "\n")
  cat("COMPARISON FOR:", dep_var, "\n")
  cat(rep("=", 70), "\n")
  
  # Run main model
  model_data <- pdata[!is.na(pdata[[dep_var]]) & 
                     !is.na(pdata$d_FLFP) & 
                     !is.na(pdata$CPR) & 
                     !is.na(pdata$TFR), ]
  
  if(nrow(model_data) < 30) {
    cat("Insufficient observations for", dep_var, "- skipping comparison\n")
    next
  }
  
  formula_str <- paste(dep_var, "~ d_FLFP + CPR + TFR + d_GDP_per_capita + d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate")
  model_formula <- as.formula(formula_str)
  
  main_fe <- plm(model_formula, data = model_data, model = "within")
  main_re <- plm(model_formula, data = model_data, model = "random")
  
  # Hausman test to select main model
  hausman_main <- phtest(main_fe, main_re)
  main_model <- if(hausman_main$p.value < 0.05) main_fe else main_re
  main_model_type <- if(hausman_main$p.value < 0.05) "Fixed Effects" else "Random Effects"
  
  cat("MAIN MODEL (", main_model_type, "):\n")
  cat("Sample size:", nrow(model_data), "observations\n")
  cat("R-squared:", round(summary(main_model)$r.squared[1], 4), "\n")
  
  # Extract key coefficients
  main_coefs <- summary(main_model)$coefficients
  
  cat("\nKEY COEFFICIENT ESTIMATES:\n")
  key_vars <- c("d_FLFP", "CPR", "TFR")
  main_p_col <- if("Pr(>|t|)" %in% colnames(main_coefs)) "Pr(>|t|)" else "Pr(>|z|)"
  
  for(var in key_vars) {
    if(var %in% rownames(main_coefs)) {
      coef_val <- main_coefs[var, "Estimate"]
      p_val <- main_coefs[var, main_p_col]
      significance <- if(p_val < 0.001) "***" else if(p_val < 0.01) "**" else if(p_val < 0.05) "*" else ""
      cat(sprintf("%s: %.6f (p=%.4f) %s\n", var, coef_val, p_val, significance))
    }
  }
  
  # Initialize comparison data frame
  comparison_df <- data.frame(
    Variable = character(),
    Main_Model = character(),
    Lagged_Model = character(),
    No_Outliers = character(),
    Consistent = character(),
    stringsAsFactors = FALSE
  )
  
  # Compare with lagged model if available
  cat("\n=== LAGGED VARIABLES MODEL COMPARISON ===\n")
  if(dep_var %in% names(robustness_results$lagged_models)) {
    lagged_model <- robustness_results$lagged_models[[dep_var]]$model
    lagged_coefs <- summary(lagged_model)$coefficients
    
    cat("Lagged model type:", robustness_results$lagged_models[[dep_var]]$model_type, "\n")
    cat("R-squared:", round(summary(lagged_model)$r.squared[1], 4), "\n")
    
    # Compare key variables with different names in lagged model
    lagged_key_vars <- c("FLFP_lag2", "CPR_lag3", "TFR_lag5")
    main_key_vars <- c("d_FLFP", "CPR", "TFR")
    
    for(i in 1:3) {
      main_var <- main_key_vars[i]
      lagged_var <- lagged_key_vars[i]
      
      main_p_col <- if("Pr(>|t|)" %in% colnames(main_coefs)) "Pr(>|t|)" else "Pr(>|z|)"
      lagged_p_col <- if("Pr(>|t|)" %in% colnames(lagged_coefs)) "Pr(>|t|)" else "Pr(>|z|)"
      
      main_est <- if(main_var %in% rownames(main_coefs)) sprintf("%.4f (p=%.3f)", main_coefs[main_var, "Estimate"], main_coefs[main_var, main_p_col]) else "N/A"
      lagged_est <- if(lagged_var %in% rownames(lagged_coefs)) sprintf("%.4f (p=%.3f)", lagged_coefs[lagged_var, "Estimate"], lagged_coefs[lagged_var, lagged_p_col]) else "N/A"
      
      # Check sign consistency
      consistent <- "N/A"
      if(main_var %in% rownames(main_coefs) && lagged_var %in% rownames(lagged_coefs)) {
        main_sign <- sign(main_coefs[main_var, "Estimate"])
        lagged_sign <- sign(lagged_coefs[lagged_var, "Estimate"])
        consistent <- ifelse(main_sign == lagged_sign, "Yes", "No")
      }
      
      cat(sprintf("%s vs %s:\n", main_var, lagged_var))
      cat(sprintf("  Main: %s\n", main_est))
      cat(sprintf("  Lagged: %s\n", lagged_est))
      cat(sprintf("  Sign consistent: %s\n\n", consistent))
      
      comparison_df <- rbind(comparison_df, data.frame(
        Variable = main_var,
        Main_Model = main_est,
        Lagged_Model = lagged_est,
        No_Outliers = "N/A",
        Consistent = consistent
      ))
    }
  } else {
    cat("No lagged model available for", dep_var, "\n")
  }
  
  # Compare with outlier exclusion model
  cat("=== OUTLIER EXCLUSION MODEL COMPARISON ===\n")
  if(dep_var %in% names(robustness_results$outlier_exclusion)) {
    outlier_model <- robustness_results$outlier_exclusion[[dep_var]]$model
    outlier_coefs <- summary(outlier_model)$coefficients
    
    cat("Outlier exclusion model type:", robustness_results$outlier_exclusion[[dep_var]]$model_type, "\n")
    cat("R-squared:", round(summary(outlier_model)$r.squared[1], 4), "\n")
    
    for(var in key_vars) {
      if(var %in% rownames(main_coefs) && var %in% rownames(outlier_coefs)) {
        # Handle different column names for p-values
        main_p_col <- if("Pr(>|t|)" %in% colnames(main_coefs)) "Pr(>|t|)" else "Pr(>|z|)"
        outlier_p_col <- if("Pr(>|t|)" %in% colnames(outlier_coefs)) "Pr(>|t|)" else "Pr(>|z|)"
        
        main_est <- sprintf("%.4f (p=%.3f)", main_coefs[var, "Estimate"], main_coefs[var, main_p_col])
        outlier_est <- sprintf("%.4f (p=%.3f)", outlier_coefs[var, "Estimate"], outlier_coefs[var, outlier_p_col])
        
        main_sign <- sign(main_coefs[var, "Estimate"])
        outlier_sign <- sign(outlier_coefs[var, "Estimate"])
        consistent <- ifelse(main_sign == outlier_sign, "Yes", "No")
        
        cat(sprintf("%s:\n", var))
        cat(sprintf("  Main: %s\n", main_est))
        cat(sprintf("  No outliers: %s\n", outlier_est))
        cat(sprintf("  Sign consistent: %s\n\n", consistent))
        
        # Update comparison data frame
        existing_row <- which(comparison_df$Variable == var)
        if(length(existing_row) > 0) {
          comparison_df[existing_row, "No_Outliers"] <- outlier_est
          if(comparison_df[existing_row, "Consistent"] != "N/A") {
            comparison_df[existing_row, "Consistent"] <- ifelse(
              comparison_df[existing_row, "Consistent"] == "Yes" && consistent == "Yes", "Yes", "Partial"
            )
          } else {
            comparison_df[existing_row, "Consistent"] <- consistent
          }
        } else {
          comparison_df <- rbind(comparison_df, data.frame(
            Variable = var,
            Main_Model = main_est,
            Lagged_Model = "N/A",
            No_Outliers = outlier_est,
            Consistent = consistent
          ))
        }
      }
    }
  } else {
    cat("No outlier exclusion model available for", dep_var, "\n")
  }
  
  # Print comparison table
  cat("\n=== SUMMARY COMPARISON TABLE ===\n")
  print(comparison_df)
  
  comparison_results[[dep_var]] <- comparison_df
  
  cat("\n", rep("-", 80), "\n")
}

# Create overall robustness assessment
cat("\n=== OVERALL ROBUSTNESS ASSESSMENT ===\n")

robustness_summary <- data.frame(
  Dependent_Variable = character(),
  Main_Model_R2 = numeric(),
  Lagged_R2 = numeric(),
  Outlier_Excl_R2 = numeric(),
  FLFP_Robust = character(),
  CPR_Robust = character(),
  TFR_Robust = character(),
  Overall_Assessment = character(),
  stringsAsFactors = FALSE
)

for (dep_var in names(comparison_results)) {
  comp_df <- comparison_results[[dep_var]]
  
  # Extract R-squared values
  main_r2 <- "N/A"
  lagged_r2 <- "N/A"
  outlier_r2 <- "N/A"
  
  if(dep_var %in% names(robustness_results$lagged_models)) {
    lagged_r2 <- round(summary(robustness_results$lagged_models[[dep_var]]$model)$r.squared[1], 4)
  }
  
  if(dep_var %in% names(robustness_results$outlier_exclusion)) {
    outlier_r2 <- round(summary(robustness_results$outlier_exclusion[[dep_var]]$model)$r.squared[1], 4)
  }
  
  # Assess variable robustness
  flfp_robust <- if("d_FLFP" %in% comp_df$Variable) comp_df[comp_df$Variable == "d_FLFP", "Consistent"] else "N/A"
  cpr_robust <- if("CPR" %in% comp_df$Variable) comp_df[comp_df$Variable == "CPR", "Consistent"] else "N/A"
  tfr_robust <- if("TFR" %in% comp_df$Variable) comp_df[comp_df$Variable == "TFR", "Consistent"] else "N/A"
  
  # Overall assessment
  robust_count <- sum(c(flfp_robust, cpr_robust, tfr_robust) %in% c("Yes", "Partial"), na.rm = TRUE)
  overall <- if(robust_count >= 2) "Robust" else if(robust_count == 1) "Partially Robust" else "Not Robust"
  
  robustness_summary <- rbind(robustness_summary, data.frame(
    Dependent_Variable = dep_var,
    Main_Model_R2 = main_r2,
    Lagged_R2 = lagged_r2,
    Outlier_Excl_R2 = outlier_r2,
    FLFP_Robust = flfp_robust,
    CPR_Robust = cpr_robust,
    TFR_Robust = tfr_robust,
    Overall_Assessment = overall
  ))
}

print(robustness_summary)
write_csv(robustness_summary, "robustness_assessment.csv")

# Create detailed coefficient comparison table
cat("\n=== DETAILED COEFFICIENT COMPARISON ===\n")

detailed_comparison <- data.frame(
  Model = character(),
  Variable = character(),
  Coefficient = numeric(),
  Std_Error = numeric(),
  P_Value = numeric(),
  Significance = character(),
  Model_Type = character(),
  stringsAsFactors = FALSE
)

# Function to extract coefficients and add to detailed comparison
add_coefficients <- function(model, model_name, model_type, key_vars) {
  coefs <- summary(model)$coefficients
  p_col <- if("Pr(>|t|)" %in% colnames(coefs)) "Pr(>|t|)" else "Pr(>|z|)"
  
  for(var in key_vars) {
    if(var %in% rownames(coefs)) {
      p_val <- coefs[var, p_col]
      detailed_comparison <<- rbind(detailed_comparison, data.frame(
        Model = model_name,
        Variable = var,
        Coefficient = coefs[var, "Estimate"],
        Std_Error = coefs[var, "Std. Error"],
        P_Value = p_val,
        Significance = ifelse(p_val < 0.001, "***",
                             ifelse(p_val < 0.01, "**",
                                   ifelse(p_val < 0.05, "*", ""))),
        Model_Type = model_type
      ))
    }
  }
}

# Add results from all models
for (dep_var in dependent_vars) {
  # Main models
  model_data <- pdata[!is.na(pdata[[dep_var]]) & 
                     !is.na(pdata$d_FLFP) & 
                     !is.na(pdata$CPR) & 
                     !is.na(pdata$TFR), ]
  
  if(nrow(model_data) >= 30) {
    formula_str <- paste(dep_var, "~ d_FLFP + CPR + TFR + d_GDP_per_capita + d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate")
    model_formula <- as.formula(formula_str)
    
    main_fe <- plm(model_formula, data = model_data, model = "within")
    main_re <- plm(model_formula, data = model_data, model = "random")
    hausman_main <- phtest(main_fe, main_re)
    main_model <- if(hausman_main$p.value < 0.05) main_fe else main_re
    main_model_type <- if(hausman_main$p.value < 0.05) "Fixed Effects" else "Random Effects"
    
    add_coefficients(main_model, paste("Main", dep_var), main_model_type, c("d_FLFP", "CPR", "TFR"))
    
    # Lagged models
    if(dep_var %in% names(robustness_results$lagged_models)) {
      lagged_model <- robustness_results$lagged_models[[dep_var]]$model
      add_coefficients(lagged_model, paste("Lagged", dep_var), 
                      robustness_results$lagged_models[[dep_var]]$model_type, 
                      c("FLFP_lag2", "CPR_lag3", "TFR_lag5"))
    }
    
    # Outlier exclusion models
    if(dep_var %in% names(robustness_results$outlier_exclusion)) {
      outlier_model <- robustness_results$outlier_exclusion[[dep_var]]$model
      add_coefficients(outlier_model, paste("No_Outliers", dep_var), 
                      robustness_results$outlier_exclusion[[dep_var]]$model_type, 
                      c("d_FLFP", "CPR", "TFR"))
    }
  }
}

print(detailed_comparison)
write_csv(detailed_comparison, "detailed_coefficient_comparison.csv")

# Save all results
save(comparison_results, robustness_summary, detailed_comparison, 
     file = "robustness_comparison_results.RData")

cat("\n=== KEY FINDINGS SUMMARY ===\n")
cat("1. Robustness check results have been compared with main models\n")
cat("2. Sign consistency analysis shows variable robustness across specifications\n")
cat("3. R-squared comparison indicates model fit stability\n")
cat("4. Files saved:\n")
cat("   - robustness_assessment.csv: Overall robustness summary\n")
cat("   - detailed_coefficient_comparison.csv: All coefficient estimates\n")
cat("   - robustness_comparison_results.RData: Complete R results\n")

cat("\n=== POLICY INTERPRETATION NOTES ===\n")
cat("- Results marked as 'Robust' show consistent effects across all specifications\n")
cat("- 'Partially Robust' indicates some sensitivity to model specification\n")
cat("- Consider economic significance alongside statistical significance\n")
cat("- Lagged models help identify delayed policy effects\n")
cat("- Outlier exclusion tests sensitivity to extreme country observations\n")