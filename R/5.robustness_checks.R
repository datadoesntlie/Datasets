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
    d_Urban_rate = Urban_rate - lag(Urban_rate),
    FLFP_lag2 = lag(FLFP, 2),
    CPR_lag3 = lag(CPR, 3),
    TFR_lag5 = lag(TFR, 5)
  ) %>%
  ungroup()

pdata <- pdata.frame(data_clean, index = c("Country", "Year"))

cat("=== ROBUSTNESS CHECKS FOR PANEL DATA MODELS ===\n\n")

dependent_vars <- c("d_Pension_GDP", "d_Old_age_dependency", 
                   "d_Pension_financing_gap", "d_Social_security_GDP")

robustness_results <- list()

cat("=== ROBUSTNESS CHECK 1: LAGGED VARIABLES MODEL ===\n")
cat("Testing with FLFP lag 2, CPR lag 3, TFR lag 5\n\n")

lagged_results <- list()

for (dep_var in dependent_vars) {
  
  cat("\n", rep("=", 60), "\n")
  cat("LAGGED MODEL FOR:", dep_var, "\n")
  cat(rep("=", 60), "\n")
  
  lagged_data <- pdata[!is.na(pdata[[dep_var]]) & 
                      !is.na(pdata$d_FLFP) & 
                      !is.na(pdata$FLFP_lag2) &
                      !is.na(pdata$CPR_lag3) & 
                      !is.na(pdata$TFR_lag5), ]
  
  if(nrow(lagged_data) < 30) {
    cat("Insufficient observations for lagged model of", dep_var, "\n")
    next
  }
  
  cat("Sample size with lags:", nrow(lagged_data), "observations\n")
  
  lagged_formula <- as.formula(paste(dep_var, "~ FLFP_lag2 + CPR_lag3 + TFR_lag5 + d_GDP_per_capita + d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate"))
  
  lagged_fe <- plm(lagged_formula, data = lagged_data, model = "within")
  lagged_re <- plm(lagged_formula, data = lagged_data, model = "random")
  
  hausman_lagged <- phtest(lagged_fe, lagged_re)
  
  if(hausman_lagged$p.value < 0.05) {
    preferred_lagged <- lagged_fe
    model_type <- "Fixed Effects"
  } else {
    preferred_lagged <- lagged_re
    model_type <- "Random Effects"
  }
  
  cat("Preferred model type:", model_type, "\n")
  cat("Hausman test p-value:", round(hausman_lagged$p.value, 4), "\n\n")
  
  print(summary(preferred_lagged))
  
  if(model_type == "Fixed Effects") {
    robust_se_lagged <- vcovHC(lagged_fe, type = "HC1")
    cat("\nRobust Standard Errors:\n")
    print(coeftest(lagged_fe, vcov = robust_se_lagged))
  }
  
  lagged_results[[dep_var]] <- list(
    model = preferred_lagged,
    hausman = hausman_lagged,
    model_type = model_type
  )
  
  cat("\n", rep("-", 70), "\n")
}

robustness_results$lagged_models <- lagged_results

cat("\n=== ROBUSTNESS CHECK 2: EXCLUDE OUTLIERS (JAPAN & CHILE) ===\n\n")

outlier_data <- pdata[!pdata$Country %in% c("Japan", "Chile"), ]
cat("Original sample size:", nrow(pdata), "observations\n")
cat("Sample size without Japan & Chile:", nrow(outlier_data), "observations\n\n")

outlier_results <- list()

for (dep_var in dependent_vars) {
  
  cat("\n", rep("=", 60), "\n")
  cat("OUTLIER EXCLUSION MODEL FOR:", dep_var, "\n")
  cat(rep("=", 60), "\n")
  
  outlier_model_data <- outlier_data[!is.na(outlier_data[[dep_var]]) & 
                                    !is.na(outlier_data$d_FLFP) & 
                                    !is.na(outlier_data$CPR) & 
                                    !is.na(outlier_data$TFR), ]
  
  if(nrow(outlier_model_data) < 30) {
    cat("Insufficient observations for outlier exclusion model of", dep_var, "\n")
    next
  }
  
  cat("Sample size without outliers:", nrow(outlier_model_data), "observations\n")
  
  outlier_formula <- as.formula(paste(dep_var, "~ d_FLFP + CPR + TFR + d_GDP_per_capita + d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate"))
  
  try({
    outlier_fe <- plm(outlier_formula, data = outlier_model_data, model = "within")
    outlier_re <- plm(outlier_formula, data = outlier_model_data, model = "random")
    
    hausman_outlier <- phtest(outlier_fe, outlier_re)
    
    if(hausman_outlier$p.value < 0.05) {
      preferred_outlier <- outlier_fe
      model_type <- "Fixed Effects"
    } else {
      preferred_outlier <- outlier_re
      model_type <- "Random Effects"
    }
  }, silent = FALSE)
  
  if(!exists("preferred_outlier")) {
    cat("Error in model estimation for", dep_var, "- using Fixed Effects only\n")
    preferred_outlier <- plm(outlier_formula, data = outlier_model_data, model = "within")
    model_type <- "Fixed Effects"
    hausman_outlier <- NULL
  }
  
  cat("Preferred model type:", model_type, "\n")
  if(!is.null(hausman_outlier)) {
    cat("Hausman test p-value:", round(hausman_outlier$p.value, 4), "\n\n")
  } else {
    cat("Hausman test not available\n\n")
  }
  
  print(summary(preferred_outlier))
  
  if(model_type == "Fixed Effects") {
    robust_se_outlier <- vcovHC(outlier_fe, type = "HC1")
    cat("\nRobust Standard Errors:\n")
    print(coeftest(outlier_fe, vcov = robust_se_outlier))
  }
  
  outlier_results[[dep_var]] <- list(
    model = preferred_outlier,
    hausman = hausman_outlier,
    model_type = model_type
  )
  
  cat("\n", rep("-", 70), "\n")
}

robustness_results$outlier_exclusion <- outlier_results

cat("\n=== ROBUSTNESS CHECK 3: SUB-SAMPLE ANALYSIS ===\n\n")

developed_countries <- c("Australia", "Austria", "Chile", "Japan", "South Korea")
developing_countries <- c("Brazil", "China", "India", "Mexico", "Russia", "South Africa")

developed_data <- pdata[pdata$Country %in% developed_countries, ]
developing_data <- pdata[pdata$Country %in% developing_countries, ]

cat("Developed countries sample size:", nrow(developed_data), "observations\n")
cat("Developing countries sample size:", nrow(developing_data), "observations\n\n")

subgroup_results <- list()

for(subgroup in c("developed", "developing")) {
  
  if(subgroup == "developed") {
    subgroup_data <- developed_data
    subgroup_name <- "DEVELOPED COUNTRIES"
  } else {
    subgroup_data <- developing_data
    subgroup_name <- "DEVELOPING COUNTRIES"
  }
  
  cat("\n", rep("=", 70), "\n")
  cat("SUB-SAMPLE ANALYSIS:", subgroup_name, "\n")
  cat(rep("=", 70), "\n")
  
  subgroup_models <- list()
  
  for (dep_var in dependent_vars) {
    
    cat("\nModel for", dep_var, "in", subgroup_name, "\n")
    cat(rep("-", 50), "\n")
    
    subgroup_model_data <- subgroup_data[!is.na(subgroup_data[[dep_var]]) & 
                                        !is.na(subgroup_data$d_FLFP) & 
                                        !is.na(subgroup_data$CPR) & 
                                        !is.na(subgroup_data$TFR), ]
    
    if(nrow(subgroup_model_data) < 20) {
      cat("Insufficient observations for", dep_var, "in", subgroup_name, "\n")
      next
    }
    
    cat("Sample size:", nrow(subgroup_model_data), "observations\n")
    
    subgroup_formula <- as.formula(paste(dep_var, "~ d_FLFP + CPR + TFR + d_GDP_per_capita + d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate"))
    
    try({
      subgroup_fe <- plm(subgroup_formula, data = subgroup_model_data, model = "within")
      subgroup_re <- plm(subgroup_formula, data = subgroup_model_data, model = "random")
      
      hausman_subgroup <- phtest(subgroup_fe, subgroup_re)
      
      if(hausman_subgroup$p.value < 0.05) {
        preferred_subgroup <- subgroup_fe
        model_type <- "Fixed Effects"
      } else {
        preferred_subgroup <- subgroup_re
        model_type <- "Random Effects"
      }
      
      cat("Preferred model type:", model_type, "\n")
      print(summary(preferred_subgroup))
      
      subgroup_models[[dep_var]] <- list(
        model = preferred_subgroup,
        hausman = hausman_subgroup,
        model_type = model_type
      )
      
    }, silent = TRUE)
  }
  
  subgroup_results[[subgroup]] <- subgroup_models
}

robustness_results$subgroup_analysis <- subgroup_results

save(robustness_results, file = "robustness_results.RData")

cat("\n=== ROBUSTNESS CHECKS SUMMARY ===\n\n")

cat("1. LAGGED VARIABLES MODEL:\n")
cat("   - Uses FLFP(t-2), CPR(t-3), TFR(t-5) to account for delayed effects\n")
cat("   - Sample sizes reduced due to lag requirements\n\n")

cat("2. OUTLIER EXCLUSION:\n")
cat("   - Excludes Japan and Chile as potential outliers\n")
cat("   - Tests robustness of results to extreme observations\n\n")

cat("3. SUB-SAMPLE ANALYSIS:\n")
cat("   - Developed countries: Australia, Austria, Chile, Japan, South Korea\n")
cat("   - Developing countries: Brazil, China, India, Mexico, Russia, South Africa\n")
cat("   - Tests if effects differ by development level\n\n")

cat("Results saved to: robustness_results.RData\n")
cat("Individual model results available in robustness_results list\n")