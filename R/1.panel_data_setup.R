library(plm)
library(dplyr)
library(readr)

data <- read_csv("R/master_dataset.csv")

data_clean <- data %>%
  filter(!is.na(Country) & !is.na(Year)) %>%
  arrange(Country, Year)

pdata <- pdata.frame(data_clean, index = c("Country", "Year"))

dependent_vars <- c("Pension_GDP", "Old_age_dependency", "Pension_financing_gap", 
                   "Social_security_GDP")

independent_vars <- c("FLFP", "CPR", "TFR", "GDP_per_capita", 
                     "Life_expectancy_65", "Female_tertiary_education", 
                     "Urban_rate")

all_vars <- c(dependent_vars, independent_vars)

stationarity_results <- list()

for (var in all_vars) {
  if (var %in% names(pdata) && sum(!is.na(pdata[[var]])) > 0) {
    
    cat("\n=== Testing stationarity for:", var, "===\n")
    
    var_data <- pdata[!is.na(pdata[[var]]), ]
    
    if (nrow(var_data) < 20) {
      cat("Insufficient observations for", var, "- skipping\n")
      next
    }
    
    test_results <- list()
    
    try({
      levin_lin <- purtest(var_data[[var]], test = "levinlin", 
                          exo = "trend", lags = "AIC", pmax = 4)
      test_results$levin_lin <- levin_lin
      cat("Levin-Lin-Chu test p-value:", levin_lin$statistic$p.value, "\n")
    }, silent = TRUE)
    
    try({
      ips <- purtest(var_data[[var]], test = "ips", 
                    exo = "trend", lags = "AIC", pmax = 4)
      test_results$ips <- ips
      cat("Im-Pesaran-Shin test p-value:", ips$statistic$p.value, "\n")
    }, silent = TRUE)
    
    try({
      madwu <- purtest(var_data[[var]], test = "madwu", 
                      exo = "trend", lags = "AIC", pmax = 4)
      test_results$madwu <- madwu
      cat("Maddala-Wu test p-value:", madwu$statistic$p.value, "\n")
    }, silent = TRUE)
    
    stationarity_results[[var]] <- test_results
  }
}

stationarity_summary <- data.frame(
  Variable = character(),
  Levin_Lin_pvalue = numeric(),
  IPS_pvalue = numeric(),
  MadWu_pvalue = numeric(),
  Stationary = character(),
  stringsAsFactors = FALSE
)

for (var in names(stationarity_results)) {
  results <- stationarity_results[[var]]
  
  ll_p <- if ("levin_lin" %in% names(results)) results$levin_lin$statistic$p.value else NA
  ips_p <- if ("ips" %in% names(results)) results$ips$statistic$p.value else NA
  mw_p <- if ("madwu" %in% names(results)) results$madwu$statistic$p.value else NA
  
  significant_tests <- sum(c(ll_p, ips_p, mw_p) < 0.05, na.rm = TRUE)
  total_tests <- sum(!is.na(c(ll_p, ips_p, mw_p)))
  
  stationary <- if (total_tests > 0) {
    if (significant_tests >= total_tests/2) "Likely Stationary" else "Likely Non-Stationary"
  } else "Cannot Determine"
  
  stationarity_summary <- rbind(stationarity_summary, data.frame(
    Variable = var,
    Levin_Lin_pvalue = ll_p,
    IPS_pvalue = ips_p,
    MadWu_pvalue = mw_p,
    Stationary = stationary
  ))
}

cat("\n=== STATIONARITY SUMMARY ===\n")
print(stationarity_summary)

write_csv(stationarity_summary, "R/stationarity_test_results_trend.csv")

non_stationary <- stationarity_summary$Variable[
  stationarity_summary$Stationary == "Likely Non-Stationary"
]

if (length(non_stationary) > 0) {
  cat("\n=== TESTING FIRST DIFFERENCES FOR NON-STATIONARY VARIABLES ===\n")
  
  for (var in non_stationary) {
    cat("\nTesting first difference of:", var, "\n")
    
    pdata_diff <- pdata %>%
      group_by(Country) %>%
      mutate(!!paste0(var, "_diff") := !!sym(var) - lag(!!sym(var))) %>%
      ungroup()
    
    diff_var <- paste0(var, "_diff")
    var_data_diff <- pdata_diff[!is.na(pdata_diff[[diff_var]]), ]
    
    if (nrow(var_data_diff) < 20) {
      cat("Insufficient observations for differenced", var, "\n")
      next
    }
    
    try({
      levin_lin_diff <- purtest(var_data_diff[[diff_var]], test = "levinlin", 
                               exo = "trend", lags = "AIC", pmax = 4)
      cat("First difference Levin-Lin-Chu p-value:", 
          levin_lin_diff$statistic$p.value, "\n")
    }, silent = TRUE)
  }
}

cat("\n=== PANEL DATA SETUP COMPLETE ===\n")
cat("Data dimensions:", nrow(pdata), "observations,", ncol(pdata), "variables\n")
cat("Countries:", length(unique(pdata$Country)), "\n")
cat("Years:", min(pdata$Year, na.rm = TRUE), "to", max(pdata$Year, na.rm = TRUE), "\n")
cat("Results saved to: R/stationarity_test_results_trend.csv\n")