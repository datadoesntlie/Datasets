library(plm)
library(dplyr)
library(readr)

data <- read_csv("master_dataset.csv")

data_clean <- data %>%
  filter(!is.na(Country) & !is.na(Year)) %>%
  arrange(Country, Year)

pdata <- pdata.frame(data_clean, index = c("Country", "Year"))

cat("=== COINTEGRATION TESTING FOR PANEL DATA ===\n\n")

dependent_vars <- c("Pension_GDP", "Old_age_dependency", "Pension_financing_gap", 
                   "Social_security_GDP")

independent_vars <- c("FLFP", "CPR", "TFR", "GDP_per_capita", 
                     "Life_expectancy_65", "Female_tertiary_education", 
                     "Urban_rate")

cointegration_results <- list()

for (dep_var in dependent_vars) {
  
  cat("\n========================================\n")
  cat("COINTEGRATION ANALYSIS FOR:", dep_var, "\n")
  cat("========================================\n")
  
  model_data <- pdata[!is.na(pdata[[dep_var]]) & 
                     !is.na(pdata$FLFP) & 
                     !is.na(pdata$CPR) & 
                     !is.na(pdata$TFR), ]
  
  if(nrow(model_data) < 50) {
    cat("Insufficient observations for", dep_var, "- skipping cointegration tests\n")
    next
  }
  
  formula_str <- paste(dep_var, "~ FLFP + CPR + TFR + GDP_per_capita + Life_expectancy_65 + Female_tertiary_education + Urban_rate")
  
  cat("Testing cointegration for model:\n")
  cat(formula_str, "\n")
  cat("Sample size:", nrow(model_data), "observations\n\n")
  
  model_formula <- as.formula(formula_str)
  
  cointegration_tests <- list()
  
  cat("=== PEDRONI COINTEGRATION TESTS ===\n")
  
  try({
    cat("Running Pedroni tests...\n")
    
    pedroni_v <- pcdtest(model_formula, data = model_data, test = "cd")
    cat("Pesaran CD test statistic:", pedroni_v$statistic, "\n")
    cat("Pesaran CD test p-value:", pedroni_v$p.value, "\n\n")
    
    cointegration_tests$pesaran_cd <- pedroni_v
    
  }, silent = FALSE)
  
  cat("=== WESTERLUND COINTEGRATION TEST ===\n")
  
  try({
    cat("Testing for cointegration using panel methods...\n")
    
    residuals_model <- plm(model_formula, data = model_data, model = "pooling")
    residuals_vec <- residuals(residuals_model)
    
    if(length(residuals_vec) > 0) {
      residuals_data <- data.frame(
        Country = model_data$Country,
        Year = model_data$Year,
        residuals = residuals_vec
      )
      
      residuals_pdata <- pdata.frame(residuals_data, index = c("Country", "Year"))
      
      if(nrow(residuals_pdata) > 20) {
        residual_unit_root <- purtest(residuals_pdata$residuals, test = "ips", 
                                     exo = "intercept", lags = "AIC", pmax = 4)
        
        cat("Residual-based cointegration test (IPS on residuals):\n")
        cat("IPS statistic:", residual_unit_root$statistic$statistic, "\n")
        cat("IPS p-value:", residual_unit_root$statistic$p.value, "\n")
        
        if(residual_unit_root$statistic$p.value < 0.05) {
          cat("*** COINTEGRATION DETECTED *** (residuals are stationary)\n")
        } else {
          cat("No cointegration found (residuals have unit root)\n")
        }
        
        cointegration_tests$residual_based <- residual_unit_root
      }
    }
    
  }, silent = FALSE)
  
  cat("\n=== ENGLE-GRANGER TWO-STEP TEST ===\n")
  
  try({
    cat("Performing Engle-Granger cointegration test...\n")
    
    long_run_model <- plm(model_formula, data = model_data, model = "pooling")
    
    cat("Long-run relationship coefficients:\n")
    print(summary(long_run_model)$coefficients)
    
    residuals_eg <- residuals(long_run_model)
    
    if(length(residuals_eg) > 20) {
      
      eg_data <- data.frame(
        Country = model_data$Country,
        Year = model_data$Year,  
        residuals = residuals_eg
      )
      
      eg_pdata <- pdata.frame(eg_data, index = c("Country", "Year"))
      
      eg_test <- purtest(eg_pdata$residuals, test = "levinlin", 
                        exo = "none", lags = "AIC", pmax = 3)
      
      cat("\nEngle-Granger Test Results:\n")
      cat("Levin-Lin-Chu statistic:", eg_test$statistic$statistic, "\n")
      cat("Levin-Lin-Chu p-value:", eg_test$statistic$p.value, "\n")
      
      if(eg_test$statistic$p.value < 0.05) {
        cat("*** COINTEGRATION CONFIRMED *** (Engle-Granger)\n")
      } else {
        cat("No cointegration (Engle-Granger)\n")
      }
      
      cointegration_tests$engle_granger <- eg_test
    }
    
  }, silent = FALSE)
  
  cointegration_results[[dep_var]] <- cointegration_tests
  
  cat("\n", rep("=", 50), "\n")
}

cat("\n=== COINTEGRATION SUMMARY ===\n")

cointegration_summary <- data.frame(
  Dependent_Variable = character(),
  Pesaran_CD_pvalue = numeric(),
  Residual_IPS_pvalue = numeric(),
  Engle_Granger_pvalue = numeric(),
  Cointegration_Evidence = character(),
  stringsAsFactors = FALSE
)

for (dep_var in names(cointegration_results)) {
  tests <- cointegration_results[[dep_var]]
  
  pesaran_p <- if("pesaran_cd" %in% names(tests)) tests$pesaran_cd$p.value else NA
  residual_p <- if("residual_based" %in% names(tests)) tests$residual_based$statistic$p.value else NA
  eg_p <- if("engle_granger" %in% names(tests)) tests$engle_granger$statistic$p.value else NA
  
  significant_tests <- sum(c(residual_p, eg_p) < 0.05, na.rm = TRUE)
  total_tests <- sum(!is.na(c(residual_p, eg_p)))
  
  cointegration_evidence <- if (total_tests > 0) {
    if (significant_tests >= 1) "Yes - Cointegration detected" else "No - No cointegration"
  } else "Inconclusive"
  
  cointegration_summary <- rbind(cointegration_summary, data.frame(
    Dependent_Variable = dep_var,
    Pesaran_CD_pvalue = pesaran_p,
    Residual_IPS_pvalue = residual_p,
    Engle_Granger_pvalue = eg_p,
    Cointegration_Evidence = cointegration_evidence
  ))
}

print(cointegration_summary)

write_csv(cointegration_summary, "cointegration_test_results.csv")

cat("\n=== MODELING RECOMMENDATIONS ===\n")

cointegrated_vars <- cointegration_summary$Dependent_Variable[
  cointegration_summary$Cointegration_Evidence == "Yes - Cointegration detected"
]

non_cointegrated_vars <- cointegration_summary$Dependent_Variable[
  cointegration_summary$Cointegration_Evidence == "No - No cointegration"
]

if(length(cointegrated_vars) > 0) {
  cat("\nVARIABLES WITH COINTEGRATION:\n")
  for(var in cointegrated_vars) {
    cat("-", var, "\n")
  }
  cat("\nRECOMMENDATION: Use Vector Error Correction Model (VECM) or Error Correction Model (ECM)\n")
  cat("This allows modeling both short-run and long-run relationships\n")
}

if(length(non_cointegrated_vars) > 0) {
  cat("\nVARIABLES WITHOUT COINTEGRATION:\n")
  for(var in non_cointegrated_vars) {
    cat("-", var, "\n")
  }
  cat("\nRECOMMENDATION: Use first-differenced models as already implemented\n")
  cat("Focus on short-run relationships only\n")
}

save(cointegration_results, file = "cointegration_results.RData")

cat("\nCointegration analysis complete!\n")
cat("Results saved to: cointegration_test_results.csv\n")
cat("Detailed results saved to: cointegration_results.RData\n")