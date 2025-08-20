library(plm)
library(lmtest)
library(sandwich)
library(stargazer)
library(dplyr)
library(readr)

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

models_results <- list()

cat("=== ECONOMETRIC MODELS: PENSION SUSTAINABILITY ===\n\n")

cat("Note: Based on stationarity tests:\n")
cat("- CPR and TFR: Used in levels (stationary)\n")
cat("- Other variables: Used in first differences (non-stationary)\n\n")

cat("=== MODEL 1: PENSION EXPENDITURE (% GDP) ===\n")
model1_data <- pdata[!is.na(pdata$d_Pension_GDP) & 
                    !is.na(pdata$d_FLFP) & 
                    !is.na(pdata$CPR) & 
                    !is.na(pdata$TFR), ]

if(nrow(model1_data) > 20) {
  model1_fe <- plm(d_Pension_GDP ~ d_FLFP + CPR + TFR + d_GDP_per_capita + 
                   d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate,
                   data = model1_data, model = "within")
  
  model1_re <- plm(d_Pension_GDP ~ d_FLFP + CPR + TFR + d_GDP_per_capita + 
                   d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate,
                   data = model1_data, model = "random")
  
  hausman1 <- phtest(model1_fe, model1_re)
  
  models_results$model1_fe <- model1_fe
  models_results$model1_re <- model1_re
  models_results$hausman1 <- hausman1
  
  cat("Fixed Effects Model:\n")
  print(summary(model1_fe))
  cat("\nRandom Effects Model:\n")
  print(summary(model1_re))
  cat("\nHausman Test:\n")
  print(hausman1)
  
  robust_se1 <- vcovHC(model1_fe, type = "HC1")
  cat("\nRobust Standard Errors (Fixed Effects):\n")
  print(coeftest(model1_fe, vcov = robust_se1))
} else {
  cat("Insufficient observations for Model 1\n")
}

cat("\n=== MODEL 2: OLD-AGE DEPENDENCY RATIO ===\n")
model2_data <- pdata[!is.na(pdata$d_Old_age_dependency) & 
                    !is.na(pdata$d_FLFP) & 
                    !is.na(pdata$CPR) & 
                    !is.na(pdata$TFR), ]

if(nrow(model2_data) > 20) {
  model2_fe <- plm(d_Old_age_dependency ~ d_FLFP + CPR + TFR + d_GDP_per_capita + 
                   d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate,
                   data = model2_data, model = "within")
  
  model2_re <- plm(d_Old_age_dependency ~ d_FLFP + CPR + TFR + d_GDP_per_capita + 
                   d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate,
                   data = model2_data, model = "random")
  
  hausman2 <- phtest(model2_fe, model2_re)
  
  models_results$model2_fe <- model2_fe
  models_results$model2_re <- model2_re
  models_results$hausman2 <- hausman2
  
  cat("Fixed Effects Model:\n")
  print(summary(model2_fe))
  cat("\nRandom Effects Model:\n")
  print(summary(model2_re))
  cat("\nHausman Test:\n")
  print(hausman2)
  
  robust_se2 <- vcovHC(model2_fe, type = "HC1")
  cat("\nRobust Standard Errors (Fixed Effects):\n")
  print(coeftest(model2_fe, vcov = robust_se2))
} else {
  cat("Insufficient observations for Model 2\n")
}

cat("\n=== MODEL 3: PENSION FINANCING GAP ===\n")
model3_data <- pdata[!is.na(pdata$d_Pension_financing_gap) & 
                    !is.na(pdata$d_FLFP) & 
                    !is.na(pdata$CPR) & 
                    !is.na(pdata$TFR), ]

if(nrow(model3_data) > 20) {
  model3_fe <- plm(d_Pension_financing_gap ~ d_FLFP + CPR + TFR + d_GDP_per_capita + 
                   d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate,
                   data = model3_data, model = "within")
  
  model3_re <- plm(d_Pension_financing_gap ~ d_FLFP + CPR + TFR + d_GDP_per_capita + 
                   d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate,
                   data = model3_data, model = "random")
  
  hausman3 <- phtest(model3_fe, model3_re)
  
  models_results$model3_fe <- model3_fe
  models_results$model3_re <- model3_re
  models_results$hausman3 <- hausman3
  
  cat("Fixed Effects Model:\n")
  print(summary(model3_fe))
  cat("\nRandom Effects Model:\n")
  print(summary(model3_re))
  cat("\nHausman Test:\n")
  print(hausman3)
  
  robust_se3 <- vcovHC(model3_fe, type = "HC1")
  cat("\nRobust Standard Errors (Fixed Effects):\n")
  print(coeftest(model3_fe, vcov = robust_se3))
} else {
  cat("Insufficient observations for Model 3\n")
}

cat("\n=== MODEL 4: SOCIAL SECURITY CONTRIBUTIONS ===\n")
model4_data <- pdata[!is.na(pdata$d_Social_security_GDP) & 
                    !is.na(pdata$d_FLFP) & 
                    !is.na(pdata$CPR) & 
                    !is.na(pdata$TFR), ]

if(nrow(model4_data) > 20) {
  model4_fe <- plm(d_Social_security_GDP ~ d_FLFP + CPR + TFR + d_GDP_per_capita + 
                   d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate,
                   data = model4_data, model = "within")
  
  model4_re <- plm(d_Social_security_GDP ~ d_FLFP + CPR + TFR + d_GDP_per_capita + 
                   d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate,
                   data = model4_data, model = "random")
  
  hausman4 <- phtest(model4_fe, model4_re)
  
  models_results$model4_fe <- model4_fe
  models_results$model4_re <- model4_re
  models_results$hausman4 <- hausman4
  
  cat("Fixed Effects Model:\n")
  print(summary(model4_fe))
  cat("\nRandom Effects Model:\n")
  print(summary(model4_re))
  cat("\nHausman Test:\n")
  print(hausman4)
  
  robust_se4 <- vcovHC(model4_fe, type = "HC1")
  cat("\nRobust Standard Errors (Fixed Effects):\n")
  print(coeftest(model4_fe, vcov = robust_se4))
} else {
  cat("Insufficient observations for Model 4\n")
}

save(models_results, file = "econometric_models_results.RData")

cat("\n=== CREATING PUBLICATION TABLES ===\n")

valid_models <- list()
model_names <- c()

if("model1_fe" %in% names(models_results)) {
  valid_models$"Pension Exp" <- models_results$model1_fe
  model_names <- c(model_names, "Pension Expenditure")
}
if("model2_fe" %in% names(models_results)) {
  valid_models$"Old Age Dep" <- models_results$model2_fe
  model_names <- c(model_names, "Old Age Dependency")
}
if("model3_fe" %in% names(models_results)) {
  valid_models$"Financing Gap" <- models_results$model3_fe
  model_names <- c(model_names, "Financing Gap")
}
if("model4_fe" %in% names(models_results)) {
  valid_models$"SS Contrib" <- models_results$model4_fe
  model_names <- c(model_names, "SS Contributions")
}

if(length(valid_models) > 0) {
  stargazer(valid_models,
            type = "text",
            title = "Panel Data Models: Pension Sustainability",
            column.labels = names(valid_models),
            covariate.labels = c("Δ Female Labor Participation",
                               "Contraceptive Prevalence Rate",
                               "Total Fertility Rate", 
                               "Δ GDP per capita",
                               "Δ Life Expectancy at 65",
                               "Δ Female Tertiary Education",
                               "Δ Urban Rate"),
            out = "econometric_results_table.txt")
  
  cat("Results table saved to: econometric_results_table.txt\n")
  cat("Model objects saved to: econometric_models_results.RData\n")
}

cat("\n=== SUMMARY OF KEY FINDINGS ===\n")
cat("Models estimated using first differences for non-stationary variables\n")
cat("and levels for stationary variables (CPR, TFR)\n")
cat("Hausman tests determine whether to use Fixed or Random Effects\n")
cat("Robust standard errors provided for Fixed Effects models\n")