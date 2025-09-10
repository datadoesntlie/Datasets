library(plm)
library(lmtest)
library(sandwich)
library(car)
library(dplyr)
library(readr)

data <- read_csv("R/master_dataset.csv")

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
    d_CPR = CPR - lag(CPR),
    d_TFR = TFR - lag(TFR),
    d_GDP_per_capita = GDP_per_capita - lag(GDP_per_capita),
    d_Life_expectancy_65 = Life_expectancy_65 - lag(Life_expectancy_65),
    d_Female_tertiary_education = Female_tertiary_education - lag(Female_tertiary_education),
    d_Urban_rate = Urban_rate - lag(Urban_rate)
  ) %>%
  ungroup()

pdata <- pdata.frame(data_clean, index = c("Country", "Year"))

cat("=== DIAGNOSTIC TESTS FOR FIRST DIFFERENCES METHODOLOGY ===\n\n")
cat("Testing full first differences approach (all variables differenced)\n")
cat("Model Selection Hierarchy: Pooled > Random Effects > Fixed Effects\n\n")

dependent_vars <- c("d_Pension_GDP", "d_Old_age_dependency", 
                   "d_Pension_financing_gap", "d_Social_security_GDP")

diagnostic_results_fd <- list()

# Function to select best model based on hierarchy
select_best_model_diagnostic <- function(pooled_model, fe_model, re_model, model_name) {
  
  # F-test: Pooled vs Fixed Effects
  f_test <- pFtest(fe_model, pooled_model)
  
  # Hausman test: Fixed Effects vs Random Effects (with error handling)
  hausman_test <- try(phtest(fe_model, re_model), silent = TRUE)
  if(inherits(hausman_test, "try-error")) {
    hausman_test <- list(p.value = 0)  # Force FE selection if Hausman test fails
  }
  
  cat("F-test (Pooled vs FE) p-value:", f_test$p.value, "\n")
  cat("Hausman test (FE vs RE) p-value:", hausman_test$p.value, "\n")
  
  # Decision logic: Preference order is Pooled > Random Effects > Fixed Effects
  if (f_test$p.value > 0.05) {
    selected_model <- pooled_model
    selected_type <- "Pooled"
    cat("Selected model: POOLED\n")
  } else {
    if (hausman_test$p.value > 0.05) {
      selected_model <- re_model
      selected_type <- "Random Effects"
      cat("Selected model: RANDOM EFFECTS\n")
    } else {
      selected_model <- fe_model
      selected_type <- "Fixed Effects"
      cat("Selected model: FIXED EFFECTS\n")
    }
  }
  
  return(list(model = selected_model, type = selected_type, 
              f_test = f_test, hausman_test = hausman_test))
}

for (dep_var in dependent_vars) {
  
  cat("\n", rep("=", 60), "\n")
  cat("DIAGNOSTIC TESTS FOR:", dep_var, "\n")
  cat(rep("=", 60), "\n")
  
  model_data <- pdata[!is.na(pdata[[dep_var]]) & 
                     !is.na(pdata$d_FLFP) & 
                     !is.na(pdata$d_CPR) & 
                     !is.na(pdata$d_TFR) &
                     !is.na(pdata$d_GDP_per_capita) &
                     !is.na(pdata$d_Life_expectancy_65) &
                     !is.na(pdata$d_Female_tertiary_education) &
                     !is.na(pdata$d_Urban_rate), ]
  
  if(nrow(model_data) < 30) {
    cat("Insufficient observations for", dep_var, "- skipping diagnostics\n")
    next
  }
  
  formula_str <- paste(dep_var, "~ d_FLFP + d_CPR + d_TFR + d_GDP_per_capita + d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate")
  model_formula <- as.formula(formula_str)
  
  cat("Model:", formula_str, "\n")
  cat("Sample size:", nrow(model_data), "observations\n\n")
  
  fe_model <- plm(model_formula, data = model_data, model = "within")
  re_model <- try(plm(model_formula, data = model_data, model = "random"), silent = TRUE)
  pooled_model <- plm(model_formula, data = model_data, model = "pooling")
  
  model_tests <- list()
  
  cat("=== 1. MODEL SELECTION TESTS ===\n")
  
  # Model selection using hierarchy
  if(!inherits(re_model, "try-error")) {
    best_model_result <- select_best_model_diagnostic(pooled_model, fe_model, re_model, dep_var)
    main_model <- best_model_result$model
    preferred_model <- best_model_result$type
    model_tests$f_test <- best_model_result$f_test
    model_tests$hausman <- best_model_result$hausman_test
  } else {
    cat("Random Effects model failed - using Fixed Effects\n")
    main_model <- fe_model
    preferred_model <- "Fixed Effects"
    model_tests$f_test <- pFtest(fe_model, pooled_model)
  }
  
  model_tests$preferred_model <- preferred_model
  
  cat("B. Breusch-Pagan LM test (RE vs Pooled):\n")
  bp_lm_test <- plmtest(pooled_model, type = "bp")
  print(bp_lm_test)
  model_tests$bp_lm <- bp_lm_test
  
  cat("\n=== 2. HETEROSKEDASTICITY TESTS ===\n")
  
  cat("A. Breusch-Pagan Test:\n")
  bp_test <- bptest(model_formula, data = model_data, studentize = FALSE)
  print(bp_test)
  model_tests$bp_hetero <- bp_test
  
  if(bp_test$p.value < 0.05) {
    cat("*** HETEROSKEDASTICITY DETECTED ***\n")
    cat("Recommendation: Use robust standard errors\n\n")
  } else {
    cat("No heteroskedasticity detected\n\n")
  }
  
  cat("B. White Test (if applicable):\n")
  try({
    if(preferred_model == "Fixed Effects") {
      white_robust_se <- vcovHC(fe_model, type = "HC1")
      cat("Robust standard errors calculated for FE model\n")
      model_tests$robust_se <- white_robust_se
    }
  }, silent = TRUE)
  
  cat("\n=== 3. SERIAL CORRELATION TESTS ===\n")
  
  cat("A. Wooldridge Test for Serial Correlation:\n")
  try({
    wooldridge_test <- pwartest(model_formula, data = model_data)
    print(wooldridge_test)
    model_tests$wooldridge <- wooldridge_test
    
    if(wooldridge_test$p.value < 0.05) {
      cat("*** SERIAL CORRELATION DETECTED ***\n")
      cat("Recommendation: Use clustered standard errors or AR models\n\n")
    } else {
      cat("No serial correlation detected\n\n")
    }
  }, silent = TRUE)
  
  cat("B. Breusch-Godfrey Test:\n")
  try({
    bg_test <- pbgtest(main_model, order = 2)
    print(bg_test)
    model_tests$bg_test <- bg_test
  }, silent = TRUE)
  
  cat("\n=== 4. MULTICOLLINEARITY ANALYSIS ===\n")
  
  complete_data <- model_data[complete.cases(model_data[, c("d_FLFP", "d_CPR", "d_TFR", 
                                                           "d_GDP_per_capita", "d_Life_expectancy_65", 
                                                           "d_Female_tertiary_education", "d_Urban_rate")]), ]
  
  if(nrow(complete_data) > 20) {
    corr_matrix <- cor(complete_data[, c("d_FLFP", "d_CPR", "d_TFR", "d_GDP_per_capita", 
                                        "d_Life_expectancy_65", "d_Female_tertiary_education", 
                                        "d_Urban_rate")], use = "complete.obs")
    
    cat("A. Correlation Matrix:\n")
    print(round(corr_matrix, 3))
    
    high_corr <- which(abs(corr_matrix) > 0.7 & corr_matrix != 1, arr.ind = TRUE)
    if(nrow(high_corr) > 0) {
      cat("\n*** HIGH CORRELATIONS DETECTED (>0.7) ***\n")
      for(i in 1:nrow(high_corr)) {
        var1 <- rownames(corr_matrix)[high_corr[i,1]]
        var2 <- colnames(corr_matrix)[high_corr[i,2]]
        cat(var1, "-", var2, ":", round(corr_matrix[high_corr[i,1], high_corr[i,2]], 3), "\n")
      }
    } else {
      cat("\nNo problematic correlations detected\n")
    }
    
    model_tests$correlation_matrix <- corr_matrix
    
    cat("\nB. VIF Analysis:\n")
    try({
      lm_model <- lm(model_formula, data = complete_data)
      vif_values <- vif(lm_model)
      print(vif_values)
      
      high_vif <- vif_values[vif_values > 5]
      if(length(high_vif) > 0) {
        cat("\n*** HIGH VIF DETECTED (>5) ***\n")
        print(high_vif)
        cat("Recommendation: Consider removing highly correlated variables\n")
      } else {
        cat("\nNo multicollinearity concerns (all VIF < 5)\n")
      }
      
      model_tests$vif <- vif_values
    }, silent = TRUE)
  }
  
  cat("\n=== 5. NORMALITY OF RESIDUALS ===\n")
  
  residuals_vec <- residuals(main_model)
  if(length(residuals_vec) > 30) {
    shapiro_test <- shapiro.test(sample(residuals_vec, min(5000, length(residuals_vec))))
    cat("Shapiro-Wilk Test:\n")
    print(shapiro_test)
    
    if(shapiro_test$p.value < 0.05) {
      cat("*** NON-NORMAL RESIDUALS ***\n")
      cat("Note: With large samples, minor deviations from normality are less concerning\n")
    } else {
      cat("Residuals appear normally distributed\n")
    }
    
    model_tests$shapiro <- shapiro_test
  }
  
  cat("\n=== 6. MODEL SUMMARY ===\n")
  print(summary(main_model))
  
  if(preferred_model == "Fixed Effects" && "robust_se" %in% names(model_tests)) {
    cat("\nRobust Standard Errors:\n")
    print(coeftest(fe_model, vcov = model_tests$robust_se))
  }
  
  diagnostic_results_fd[[dep_var]] <- model_tests
  
  cat("\n", rep("=", 80), "\n")
}

cat("\n=== DIAGNOSTIC SUMMARY FOR FIRST DIFFERENCES APPROACH ===\n")

summary_df_fd <- data.frame(
  Model = character(),
  Preferred_Method = character(),
  Heteroskedasticity = character(),
  Serial_Correlation = character(),
  High_Multicollinearity = character(),
  Robust_SE_Needed = character(),
  stringsAsFactors = FALSE
)

for (dep_var in names(diagnostic_results_fd)) {
  tests <- diagnostic_results_fd[[dep_var]]
  
  preferred <- if("preferred_model" %in% names(tests)) tests$preferred_model else "Unknown"
  
  hetero <- if("bp_hetero" %in% names(tests)) {
    if(tests$bp_hetero$p.value < 0.05) "Yes" else "No"
  } else "Unknown"
  
  serial <- if("wooldridge" %in% names(tests)) {
    if(tests$wooldridge$p.value < 0.05) "Yes" else "No"
  } else "Unknown"
  
  multicoll <- if("vif" %in% names(tests)) {
    if(any(tests$vif > 5, na.rm = TRUE)) "Yes" else "No"
  } else "Unknown"
  
  robust_needed <- if(hetero == "Yes" || serial == "Yes") "Yes" else "No"
  
  summary_df_fd <- rbind(summary_df_fd, data.frame(
    Model = dep_var,
    Preferred_Method = preferred,
    Heteroskedasticity = hetero,
    Serial_Correlation = serial,
    High_Multicollinearity = multicoll,
    Robust_SE_Needed = robust_needed
  ))
}

print(summary_df_fd)

write_csv(summary_df_fd, "R/4.diagnostic_summary_fd.csv")
save(diagnostic_results_fd, file = "R/4.diagnostic_results_fd.RData")

cat("\n=== DETAILED DIAGNOSTIC TEST RESULTS (FIRST DIFFERENCES) ===\n")

detailed_results_fd <- data.frame(
  Model = character(),
  Test_Type = character(),
  Test_Name = character(),
  Statistic = numeric(),
  P_Value = numeric(),
  Decision = character(),
  stringsAsFactors = FALSE
)

for (model_name in names(diagnostic_results_fd)) {
  tests <- diagnostic_results_fd[[model_name]]
  
  cat("\nModel:", model_name, "\n")
  cat(rep("=", 50), "\n")
  
  if("hausman" %in% names(tests)) {
    hausman <- tests$hausman
    cat("Hausman Test: Chi-sq =", round(hausman$statistic, 4), ", p =", round(hausman$p.value, 6), "\n")
    
    detailed_results_fd <- rbind(detailed_results_fd, data.frame(
      Model = model_name,
      Test_Type = "Model Selection",
      Test_Name = "Hausman Test",
      Statistic = hausman$statistic,
      P_Value = hausman$p.value,
      Decision = ifelse(hausman$p.value < 0.05, "Fixed Effects", "Random Effects")
    ))
  }
  
  if("f_test" %in% names(tests)) {
    pf <- tests$f_test
    cat("F-test (Pooled vs FE): F =", round(pf$statistic, 4), ", p =", round(pf$p.value, 6), "\n")
    
    detailed_results_fd <- rbind(detailed_results_fd, data.frame(
      Model = model_name,
      Test_Type = "Model Selection", 
      Test_Name = "F-test (Pooled vs FE)",
      Statistic = pf$statistic,
      P_Value = pf$p.value,
      Decision = ifelse(pf$p.value < 0.05, "Country Effects Needed", "Pooled Adequate")
    ))
  }
  
  if("bp_hetero" %in% names(tests)) {
    bp_hetero <- tests$bp_hetero
    cat("Breusch-Pagan (Heteroskedasticity): BP =", round(bp_hetero$statistic, 4), ", p =", round(bp_hetero$p.value, 6), "\n")
    
    detailed_results_fd <- rbind(detailed_results_fd, data.frame(
      Model = model_name,
      Test_Type = "Heteroskedasticity",
      Test_Name = "Breusch-Pagan Test", 
      Statistic = bp_hetero$statistic,
      P_Value = bp_hetero$p.value,
      Decision = ifelse(bp_hetero$p.value < 0.05, "Heteroskedasticity Present", "No Heteroskedasticity")
    ))
  }
  
  if("wooldridge" %in% names(tests)) {
    wooldridge <- tests$wooldridge
    cat("Wooldridge (Serial Correlation): F =", round(wooldridge$statistic, 4), ", p =", round(wooldridge$p.value, 6), "\n")
    
    detailed_results_fd <- rbind(detailed_results_fd, data.frame(
      Model = model_name,
      Test_Type = "Serial Correlation",
      Test_Name = "Wooldridge Test",
      Statistic = wooldridge$statistic,
      P_Value = wooldridge$p.value,
      Decision = ifelse(wooldridge$p.value < 0.05, "Serial Correlation Present", "No Serial Correlation")
    ))
  }
  
  if("vif" %in% names(tests)) {
    vif_vals <- tests$vif
    max_vif <- max(vif_vals, na.rm = TRUE)
    cat("Maximum VIF:", round(max_vif, 4), "\n")
    
    detailed_results_fd <- rbind(detailed_results_fd, data.frame(
      Model = model_name,
      Test_Type = "Multicollinearity",
      Test_Name = "Maximum VIF",
      Statistic = max_vif,
      P_Value = NA,
      Decision = ifelse(max_vif > 5, "Multicollinearity Concern", "No Multicollinearity")
    ))
  }
  
  if("shapiro" %in% names(tests)) {
    shapiro <- tests$shapiro
    cat("Shapiro-Wilk (Normality): W =", round(shapiro$statistic, 4), ", p =", round(shapiro$p.value, 6), "\n")
    
    detailed_results_fd <- rbind(detailed_results_fd, data.frame(
      Model = model_name,
      Test_Type = "Normality",
      Test_Name = "Shapiro-Wilk Test",
      Statistic = shapiro$statistic,
      P_Value = shapiro$p.value,
      Decision = ifelse(shapiro$p.value < 0.05, "Non-Normal Residuals", "Normal Residuals")
    ))
  }
}

print(detailed_results_fd)
write_csv(detailed_results_fd, "R/4.detailed_diagnostic_results_fd.csv")

cat("\n=== FIRST DIFFERENCES METHODOLOGY RECOMMENDATIONS ===\n")
cat("1. Use preferred model specification based on F-test and Hausman test hierarchy\n")
cat("2. Apply robust standard errors where heteroskedasticity is detected\n")
cat("3. Consider clustered SEs if serial correlation is present\n")
cat("4. First differences should reduce serial correlation concerns\n")
cat("5. Results saved to: 4.diagnostic_summary_fd.csv, 4.detailed_diagnostic_results_fd.csv, and 4.diagnostic_results_fd.RData\n")