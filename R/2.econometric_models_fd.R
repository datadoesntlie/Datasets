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

models_results <- list()

cat("=== ECONOMETRIC MODELS: PENSION SUSTAINABILITY (FULL FIRST DIFFERENCES) ===\n\n")

cat("Note: All variables used in first differences\n\n")

# Function to select best model based on tests
select_best_model <- function(pooled_model, fe_model, re_model, model_name) {
  
  # F-test: Pooled vs Fixed Effects
  f_test <- pFtest(fe_model, pooled_model)
  
  # Hausman test: Fixed Effects vs Random Effects (with error handling)
  if(inherits(re_model, "try-error")) {
    hausman_test <- list(p.value = 0)  # Force FE selection if RE fails
    cat("Random Effects model failed - selecting Fixed Effects\n")
  } else {
    hausman_test <- try(phtest(fe_model, re_model), silent = TRUE)
    if(inherits(hausman_test, "try-error")) {
      hausman_test <- list(p.value = 0)  # Force FE selection if Hausman test fails
    }
  }
  
  cat("\n--- Model Selection Tests for", model_name, "---\n")
  cat("F-test (Pooled vs FE) p-value:", f_test$p.value, "\n")
  cat("Hausman test (FE vs RE) p-value:", hausman_test$p.value, "\n")
  
  # Decision logic: Preference order is Pooled > Random Effects > Fixed Effects
  if (f_test$p.value > 0.05) {
    # F-test not significant: Pooled is preferred
    selected_model <- pooled_model
    selected_type <- "Pooled"
    cat("Selected model: POOLED (F-test not significant - no country effects needed)\n")
  } else {
    # F-test significant: Country effects are needed, choose between RE and FE
    if (hausman_test$p.value > 0.05) {
      # Hausman test not significant: RE is preferred (more efficient)
      selected_model <- re_model
      selected_type <- "Random Effects"
      cat("Selected model: RANDOM EFFECTS (F-test significant, but Hausman test not significant)\n")
    } else {
      # Hausman test significant: FE is required (RE assumptions violated)
      selected_model <- fe_model
      selected_type <- "Fixed Effects"
      cat("Selected model: FIXED EFFECTS (both F-test and Hausman test significant)\n")
    }
  }
  
  return(list(model = selected_model, type = selected_type, 
              f_test = f_test, hausman_test = hausman_test))
}

cat("=== MODEL 1: PENSION EXPENDITURE (% GDP) ===\n")
model1_data <- pdata[!is.na(pdata$d_Pension_GDP) & 
                    !is.na(pdata$d_FLFP) & 
                    !is.na(pdata$d_CPR) & 
                    !is.na(pdata$d_TFR), ]

if(nrow(model1_data) > 20) {
  
  # Pooled OLS
  model1_pooled <- plm(d_Pension_GDP ~ d_FLFP + d_CPR + d_TFR + d_GDP_per_capita + 
                       d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate,
                       data = model1_data, model = "pooling")
  
  # Fixed Effects
  model1_fe <- plm(d_Pension_GDP ~ d_FLFP + d_CPR + d_TFR + d_GDP_per_capita + 
                   d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate,
                   data = model1_data, model = "within")
  
  # Random Effects (with error handling)
  model1_re <- try(plm(d_Pension_GDP ~ d_FLFP + d_CPR + d_TFR + d_GDP_per_capita + 
                       d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate,
                       data = model1_data, model = "random"), silent = TRUE)
  
  # Model selection
  model1_best <- select_best_model(model1_pooled, model1_fe, model1_re, "Pension Expenditure")
  
  # Store results
  models_results$model1_pooled <- model1_pooled
  models_results$model1_fe <- model1_fe
  models_results$model1_re <- model1_re
  models_results$model1_best <- model1_best$model
  models_results$model1_type <- model1_best$type
  models_results$model1_tests <- list(f_test = model1_best$f_test, 
                                     hausman_test = model1_best$hausman_test)
  
  cat("\nSelected Model Summary:\n")
  print(summary(model1_best$model))
  
  # Robust standard errors if Fixed Effects
  if(model1_best$type == "Fixed Effects") {
    robust_se1 <- vcovHC(model1_best$model, type = "HC1")
    cat("\nRobust Standard Errors:\n")
    print(coeftest(model1_best$model, vcov = robust_se1))
  }
  
} else {
  cat("Insufficient observations for Model 1\n")
}

cat("\n=== MODEL 2: OLD-AGE DEPENDENCY RATIO ===\n")
model2_data <- pdata[!is.na(pdata$d_Old_age_dependency) & 
                    !is.na(pdata$d_FLFP) & 
                    !is.na(pdata$d_CPR) & 
                    !is.na(pdata$d_TFR), ]

if(nrow(model2_data) > 20) {
  
  # Pooled OLS
  model2_pooled <- plm(d_Old_age_dependency ~ d_FLFP + d_CPR + d_TFR + d_GDP_per_capita + 
                       d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate,
                       data = model2_data, model = "pooling")
  
  # Fixed Effects
  model2_fe <- plm(d_Old_age_dependency ~ d_FLFP + d_CPR + d_TFR + d_GDP_per_capita + 
                   d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate,
                   data = model2_data, model = "within")
  
  # Random Effects (with error handling)
  model2_re <- try(plm(d_Old_age_dependency ~ d_FLFP + d_CPR + d_TFR + d_GDP_per_capita + 
                       d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate,
                       data = model2_data, model = "random"), silent = TRUE)
  
  # Model selection
  model2_best <- select_best_model(model2_pooled, model2_fe, model2_re, "Old-Age Dependency")
  
  # Store results
  models_results$model2_pooled <- model2_pooled
  models_results$model2_fe <- model2_fe
  models_results$model2_re <- model2_re
  models_results$model2_best <- model2_best$model
  models_results$model2_type <- model2_best$type
  models_results$model2_tests <- list(f_test = model2_best$f_test,
                                     hausman_test = model2_best$hausman_test)
  
  cat("\nSelected Model Summary:\n")
  print(summary(model2_best$model))
  
  # Robust standard errors if Fixed Effects
  if(model2_best$type == "Fixed Effects") {
    robust_se2 <- vcovHC(model2_best$model, type = "HC1")
    cat("\nRobust Standard Errors:\n")
    print(coeftest(model2_best$model, vcov = robust_se2))
  }
  
} else {
  cat("Insufficient observations for Model 2\n")
}

cat("\n=== MODEL 3: PENSION FINANCING GAP ===\n")
model3_data <- pdata[!is.na(pdata$d_Pension_financing_gap) & 
                    !is.na(pdata$d_FLFP) & 
                    !is.na(pdata$d_CPR) & 
                    !is.na(pdata$d_TFR), ]

if(nrow(model3_data) > 20) {
  
  # Pooled OLS
  model3_pooled <- plm(d_Pension_financing_gap ~ d_FLFP + d_CPR + d_TFR + d_GDP_per_capita + 
                       d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate,
                       data = model3_data, model = "pooling")
  
  # Fixed Effects
  model3_fe <- plm(d_Pension_financing_gap ~ d_FLFP + d_CPR + d_TFR + d_GDP_per_capita + 
                   d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate,
                   data = model3_data, model = "within")
  
  # Random Effects (with error handling)
  model3_re <- try(plm(d_Pension_financing_gap ~ d_FLFP + d_CPR + d_TFR + d_GDP_per_capita + 
                       d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate,
                       data = model3_data, model = "random"), silent = TRUE)
  
  # Model selection
  model3_best <- select_best_model(model3_pooled, model3_fe, model3_re, "Pension Financing Gap")
  
  # Store results
  models_results$model3_pooled <- model3_pooled
  models_results$model3_fe <- model3_fe
  models_results$model3_re <- model3_re
  models_results$model3_best <- model3_best$model
  models_results$model3_type <- model3_best$type
  models_results$model3_tests <- list(f_test = model3_best$f_test,
                                     hausman_test = model3_best$hausman_test)
  
  cat("\nSelected Model Summary:\n")
  print(summary(model3_best$model))
  
  # Robust standard errors if Fixed Effects
  if(model3_best$type == "Fixed Effects") {
    robust_se3 <- vcovHC(model3_best$model, type = "HC1")
    cat("\nRobust Standard Errors:\n")
    print(coeftest(model3_best$model, vcov = robust_se3))
  }
  
} else {
  cat("Insufficient observations for Model 3\n")
}

cat("\n=== MODEL 4: SOCIAL SECURITY CONTRIBUTIONS ===\n")
model4_data <- pdata[!is.na(pdata$d_Social_security_GDP) & 
                    !is.na(pdata$d_FLFP) & 
                    !is.na(pdata$d_CPR) & 
                    !is.na(pdata$d_TFR), ]

if(nrow(model4_data) > 20) {
  
  # Pooled OLS
  model4_pooled <- plm(d_Social_security_GDP ~ d_FLFP + d_CPR + d_TFR + d_GDP_per_capita + 
                       d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate,
                       data = model4_data, model = "pooling")
  
  # Fixed Effects
  model4_fe <- plm(d_Social_security_GDP ~ d_FLFP + d_CPR + d_TFR + d_GDP_per_capita + 
                   d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate,
                   data = model4_data, model = "within")
  
  # Random Effects (with error handling)
  model4_re <- try(plm(d_Social_security_GDP ~ d_FLFP + d_CPR + d_TFR + d_GDP_per_capita + 
                       d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate,
                       data = model4_data, model = "random"), silent = TRUE)
  
  # Model selection
  model4_best <- select_best_model(model4_pooled, model4_fe, model4_re, "Social Security Contributions")
  
  # Store results
  models_results$model4_pooled <- model4_pooled
  models_results$model4_fe <- model4_fe
  models_results$model4_re <- model4_re
  models_results$model4_best <- model4_best$model
  models_results$model4_type <- model4_best$type
  models_results$model4_tests <- list(f_test = model4_best$f_test,
                                     hausman_test = model4_best$hausman_test)
  
  cat("\nSelected Model Summary:\n")
  print(summary(model4_best$model))
  
  # Robust standard errors if Fixed Effects
  if(model4_best$type == "Fixed Effects") {
    robust_se4 <- vcovHC(model4_best$model, type = "HC1")
    cat("\nRobust Standard Errors:\n")
    print(coeftest(model4_best$model, vcov = robust_se4))
  }
  
} else {
  cat("Insufficient observations for Model 4\n")
}

save(models_results, file = "econometric_models_fd_results.RData")

cat("\n=== CREATING PUBLICATION TABLES ===\n")

selected_models <- list()
model_types <- c()

if("model1_best" %in% names(models_results)) {
  selected_models$"Pension Exp" <- models_results$model1_best
  model_types <- c(model_types, paste0("Pension Exp (", models_results$model1_type, ")"))
}
if("model2_best" %in% names(models_results)) {
  selected_models$"Old Age Dep" <- models_results$model2_best
  model_types <- c(model_types, paste0("Old Age Dep (", models_results$model2_type, ")"))
}
if("model3_best" %in% names(models_results)) {
  selected_models$"Financing Gap" <- models_results$model3_best
  model_types <- c(model_types, paste0("Financing Gap (", models_results$model3_type, ")"))
}
if("model4_best" %in% names(models_results)) {
  selected_models$"SS Contrib" <- models_results$model4_best
  model_types <- c(model_types, paste0("SS Contrib (", models_results$model4_type, ")"))
}

if(length(selected_models) > 0) {
  
  # Create column labels with model types
  column_labels_with_types <- c()
  if("model1_best" %in% names(models_results)) {
    column_labels_with_types <- c(column_labels_with_types, 
                                 paste0("Pension Exp (", models_results$model1_type, ")"))
  }
  if("model2_best" %in% names(models_results)) {
    column_labels_with_types <- c(column_labels_with_types, 
                                 paste0("Old Age Dep (", models_results$model2_type, ")"))
  }
  if("model3_best" %in% names(models_results)) {
    column_labels_with_types <- c(column_labels_with_types, 
                                 paste0("Financing Gap (", models_results$model3_type, ")"))
  }
  if("model4_best" %in% names(models_results)) {
    column_labels_with_types <- c(column_labels_with_types, 
                                 paste0("SS Contrib (", models_results$model4_type, ")"))
  }
  
  stargazer(selected_models,
            type = "text",
            title = "Panel Data Models: Pension Sustainability (Full First Differences)",
            column.labels = column_labels_with_types,
            covariate.labels = c("Δ Female Labor Participation",
                               "Δ Contraceptive Prevalence Rate",
                               "Δ Total Fertility Rate", 
                               "Δ GDP per capita",
                               "Δ Life Expectancy at 65",
                               "Δ Female Tertiary Education",
                               "Δ Urban Rate"),
            out = "R/econometric_fd_results_table.txt")
  
  cat("Results table saved to: R/econometric_fd_results_table.txt\n")
  cat("Model objects saved to: econometric_models_fd_results.RData\n")
}

cat("\n=== SUMMARY OF FULL FIRST DIFFERENCES APPROACH ===\n")
cat("All variables used in first differences (including CPR and TFR)\n")
cat("Model selection hierarchy: Pooled > Random Effects > Fixed Effects\n")
cat("F-test (Pooled vs FE) and Hausman test (FE vs RE) determine selection\n")
cat("Selected model types:\n")
for(i in 1:length(model_types)) {
  cat("-", model_types[i], "\n")
}