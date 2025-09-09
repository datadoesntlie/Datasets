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
  mutate(
    # Center the year variable to reduce multicollinearity
    Year_centered = Year - mean(Year, na.rm = TRUE),
    Year_squared = Year_centered^2
  )

pdata <- pdata.frame(data_clean, index = c("Country", "Year"))

models_results_levels <- list()

cat("=== ECONOMETRIC MODELS: PENSION SUSTAINABILITY (LEVELS APPROACH) ===\n\n")

cat("Model Specification: All variables in levels with linear and quadratic time trends\n")
cat("PensionSustainability_it = β₀ + β₁FLFP_it + β₂CPR_it + β₃TFR_it + β₄GDP_pc_it +\n")
cat("                          β₅Life_Expect65_it + β₆FTE_it + β₇Urban_it + β₈Year_c + β₉Year_c² + μᵢ + εᵢt\n\n")

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

# MODEL 1: PENSION EXPENDITURE (% GDP)
cat("\n=== MODEL 1: PENSION EXPENDITURE (% GDP) ===\n")
model1_data <- pdata[!is.na(pdata$Pension_GDP) & 
                    !is.na(pdata$FLFP) & 
                    !is.na(pdata$CPR) & 
                    !is.na(pdata$TFR) &
                    !is.na(pdata$GDP_per_capita) &
                    !is.na(pdata$Life_expectancy_65) &
                    !is.na(pdata$Female_tertiary_education) &
                    !is.na(pdata$Urban_rate), ]

if(nrow(model1_data) > 20) {
  
  # Pooled OLS
  model1_pooled <- plm(Pension_GDP ~ FLFP + CPR + TFR + GDP_per_capita + 
                       Life_expectancy_65 + Female_tertiary_education + Urban_rate +
                       Year_centered + Year_squared,
                       data = model1_data, model = "pooling")
  
  # Fixed Effects
  model1_fe <- plm(Pension_GDP ~ FLFP + CPR + TFR + GDP_per_capita + 
                   Life_expectancy_65 + Female_tertiary_education + Urban_rate +
                   Year_centered + Year_squared,
                   data = model1_data, model = "within")
  
  # Random Effects (with error handling)
  model1_re <- try(plm(Pension_GDP ~ FLFP + CPR + TFR + GDP_per_capita + 
                       Life_expectancy_65 + Female_tertiary_education + Urban_rate +
                       Year_centered + Year_squared,
                       data = model1_data, model = "random"), silent = TRUE)
  
  # Model selection
  model1_best <- select_best_model(model1_pooled, model1_fe, model1_re, "Pension Expenditure")
  
  # Store results
  models_results_levels$model1_pooled <- model1_pooled
  models_results_levels$model1_fe <- model1_fe
  models_results_levels$model1_re <- model1_re
  models_results_levels$model1_best <- model1_best$model
  models_results_levels$model1_type <- model1_best$type
  models_results_levels$model1_tests <- list(f_test = model1_best$f_test, 
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

# MODEL 2: OLD-AGE DEPENDENCY RATIO
cat("\n=== MODEL 2: OLD-AGE DEPENDENCY RATIO ===\n")
model2_data <- pdata[!is.na(pdata$Old_age_dependency) & 
                    !is.na(pdata$FLFP) & 
                    !is.na(pdata$CPR) & 
                    !is.na(pdata$TFR) &
                    !is.na(pdata$GDP_per_capita) &
                    !is.na(pdata$Life_expectancy_65) &
                    !is.na(pdata$Female_tertiary_education) &
                    !is.na(pdata$Urban_rate), ]

if(nrow(model2_data) > 20) {
  
  # Pooled OLS
  model2_pooled <- plm(Old_age_dependency ~ FLFP + CPR + TFR + GDP_per_capita + 
                       Life_expectancy_65 + Female_tertiary_education + Urban_rate +
                       Year_centered + Year_squared,
                       data = model2_data, model = "pooling")
  
  # Fixed Effects
  model2_fe <- plm(Old_age_dependency ~ FLFP + CPR + TFR + GDP_per_capita + 
                   Life_expectancy_65 + Female_tertiary_education + Urban_rate +
                   Year_centered + Year_squared,
                   data = model2_data, model = "within")
  
  # Random Effects (with error handling)
  model2_re <- try(plm(Old_age_dependency ~ FLFP + CPR + TFR + GDP_per_capita + 
                       Life_expectancy_65 + Female_tertiary_education + Urban_rate +
                       Year_centered + Year_squared,
                       data = model2_data, model = "random"), silent = TRUE)
  
  # Model selection
  model2_best <- select_best_model(model2_pooled, model2_fe, model2_re, "Old-Age Dependency")
  
  # Store results
  models_results_levels$model2_pooled <- model2_pooled
  models_results_levels$model2_fe <- model2_fe
  models_results_levels$model2_re <- model2_re
  models_results_levels$model2_best <- model2_best$model
  models_results_levels$model2_type <- model2_best$type
  models_results_levels$model2_tests <- list(f_test = model2_best$f_test,
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

# MODEL 3: PENSION FINANCING GAP
cat("\n=== MODEL 3: PENSION FINANCING GAP ===\n")
model3_data <- pdata[!is.na(pdata$Pension_financing_gap) & 
                    !is.na(pdata$FLFP) & 
                    !is.na(pdata$CPR) & 
                    !is.na(pdata$TFR) &
                    !is.na(pdata$GDP_per_capita) &
                    !is.na(pdata$Life_expectancy_65) &
                    !is.na(pdata$Female_tertiary_education) &
                    !is.na(pdata$Urban_rate), ]

if(nrow(model3_data) > 20) {
  
  # Pooled OLS
  model3_pooled <- plm(Pension_financing_gap ~ FLFP + CPR + TFR + GDP_per_capita + 
                       Life_expectancy_65 + Female_tertiary_education + Urban_rate +
                       Year_centered + Year_squared,
                       data = model3_data, model = "pooling")
  
  # Fixed Effects
  model3_fe <- plm(Pension_financing_gap ~ FLFP + CPR + TFR + GDP_per_capita + 
                   Life_expectancy_65 + Female_tertiary_education + Urban_rate +
                   Year_centered + Year_squared,
                   data = model3_data, model = "within")
  
  # Random Effects (with error handling)  
  model3_re <- try(plm(Pension_financing_gap ~ FLFP + CPR + TFR + GDP_per_capita + 
                       Life_expectancy_65 + Female_tertiary_education + Urban_rate +
                       Year_centered + Year_squared,
                       data = model3_data, model = "random"), silent = TRUE)
  
  # Model selection
  model3_best <- select_best_model(model3_pooled, model3_fe, model3_re, "Pension Financing Gap")
  
  # Store results
  models_results_levels$model3_pooled <- model3_pooled
  models_results_levels$model3_fe <- model3_fe
  models_results_levels$model3_re <- model3_re
  models_results_levels$model3_best <- model3_best$model
  models_results_levels$model3_type <- model3_best$type
  models_results_levels$model3_tests <- list(f_test = model3_best$f_test,
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

# MODEL 4: SOCIAL SECURITY CONTRIBUTIONS
cat("\n=== MODEL 4: SOCIAL SECURITY CONTRIBUTIONS ===\n")
model4_data <- pdata[!is.na(pdata$Social_security_GDP) & 
                    !is.na(pdata$FLFP) & 
                    !is.na(pdata$CPR) & 
                    !is.na(pdata$TFR) &
                    !is.na(pdata$GDP_per_capita) &
                    !is.na(pdata$Life_expectancy_65) &
                    !is.na(pdata$Female_tertiary_education) &
                    !is.na(pdata$Urban_rate), ]

if(nrow(model4_data) > 20) {
  
  # Pooled OLS
  model4_pooled <- plm(Social_security_GDP ~ FLFP + CPR + TFR + GDP_per_capita + 
                       Life_expectancy_65 + Female_tertiary_education + Urban_rate +
                       Year_centered + Year_squared,
                       data = model4_data, model = "pooling")
  
  # Fixed Effects
  model4_fe <- plm(Social_security_GDP ~ FLFP + CPR + TFR + GDP_per_capita + 
                   Life_expectancy_65 + Female_tertiary_education + Urban_rate +
                   Year_centered + Year_squared,
                   data = model4_data, model = "within")
  
  # Random Effects (with error handling)
  model4_re <- try(plm(Social_security_GDP ~ FLFP + CPR + TFR + GDP_per_capita + 
                       Life_expectancy_65 + Female_tertiary_education + Urban_rate +
                       Year_centered + Year_squared,
                       data = model4_data, model = "random"), silent = TRUE)
  
  # Model selection
  model4_best <- select_best_model(model4_pooled, model4_fe, model4_re, "Social Security Contributions")
  
  # Store results
  models_results_levels$model4_pooled <- model4_pooled
  models_results_levels$model4_fe <- model4_fe
  models_results_levels$model4_re <- model4_re
  models_results_levels$model4_best <- model4_best$model
  models_results_levels$model4_type <- model4_best$type
  models_results_levels$model4_tests <- list(f_test = model4_best$f_test,
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

# Save results
save(models_results_levels, file = "R/econometric_models_levels_results.RData")

# Create publication table with selected models
cat("\n=== CREATING PUBLICATION TABLES ===\n")

selected_models <- list()
model_types <- c()

if("model1_best" %in% names(models_results_levels)) {
  selected_models$"Pension Exp" <- models_results_levels$model1_best
  model_types <- c(model_types, paste0("Pension Exp (", models_results_levels$model1_type, ")"))
}
if("model2_best" %in% names(models_results_levels)) {
  selected_models$"Old Age Dep" <- models_results_levels$model2_best
  model_types <- c(model_types, paste0("Old Age Dep (", models_results_levels$model2_type, ")"))
}
if("model3_best" %in% names(models_results_levels)) {
  selected_models$"Financing Gap" <- models_results_levels$model3_best
  model_types <- c(model_types, paste0("Financing Gap (", models_results_levels$model3_type, ")"))
}
if("model4_best" %in% names(models_results_levels)) {
  selected_models$"SS Contrib" <- models_results_levels$model4_best
  model_types <- c(model_types, paste0("SS Contrib (", models_results_levels$model4_type, ")"))
}

if(length(selected_models) > 0) {
  # Create column labels with model types
  column_labels_with_types <- c()
  if("model1_best" %in% names(models_results_levels)) {
    column_labels_with_types <- c(column_labels_with_types, 
                                 paste0("Pension Exp (", models_results_levels$model1_type, ")"))
  }
  if("model2_best" %in% names(models_results_levels)) {
    column_labels_with_types <- c(column_labels_with_types, 
                                 paste0("Old Age Dep (", models_results_levels$model2_type, ")"))
  }
  if("model3_best" %in% names(models_results_levels)) {
    column_labels_with_types <- c(column_labels_with_types, 
                                 paste0("Financing Gap (", models_results_levels$model3_type, ")"))
  }
  if("model4_best" %in% names(models_results_levels)) {
    column_labels_with_types <- c(column_labels_with_types, 
                                 paste0("SS Contrib (", models_results_levels$model4_type, ")"))
  }
  
  stargazer(selected_models,
            type = "text",
            title = "Panel Data Models: Pension Sustainability (Levels with Time Trends)",
            column.labels = column_labels_with_types,
            covariate.labels = c("Female Labor Participation",
                               "Contraceptive Prevalence Rate", 
                               "Total Fertility Rate",
                               "GDP per capita",
                               "Life Expectancy at 65",
                               "Female Tertiary Education",
                               "Urban Rate",
                               "Year Centered",
                               "Year Squared"),
            out = "R/econometric_levels_results_table.txt")
  
  cat("Results table saved to: R/econometric_levels_results_table.txt\n")
  cat("Model objects saved to: R/econometric_models_levels_results.RData\n")
}

cat("\n=== SUMMARY OF LEVELS APPROACH ===\n")
cat("Models estimated using all variables in levels\n")
cat("Time trends included: Linear (Year centered) and Quadratic (Year centered²)\n")
cat("Model selection hierarchy: Pooled > Random Effects > Fixed Effects\n")
cat("F-test (Pooled vs FE) and Hausman test (FE vs RE) determine selection\n")
cat("Selected model types:\n")
for(i in 1:length(model_types)) {
  cat("-", model_types[i], "\n")
}