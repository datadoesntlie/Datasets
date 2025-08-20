# Create Summary Statistics for Data and Descriptive Statistics Chapter
# Section 3.3: Summary Statistics

library(dplyr)
library(readr)
library(tidyr)

print("Creating comprehensive summary statistics...")

# Define country classifications
developed_countries <- c("Germany", "France", "Sweden", "Italy", "Spain", "Greece", "Japan", "South Korea")
developing_countries <- c("Brazil", "Mexico", "Chile")

# Read all cleaned datasets
datasets <- list(
  contraceptive = read_csv("R/contraceptive_clean.csv"),
  female_labor = read_csv("R/female_labor_clean.csv"),
  female_education = read_csv("R/Female_tertiary_education_merged_clean.csv"),
  fertility = read_csv("R/fertility_clean.csv"),
  gdp = read_csv("R/gdp_clean.csv"),
  life_expectancy = read_csv("R/Life_Expectancy_65_clean.csv"),
  old_age_dependency = read_csv("R/Old_Age_Dependancy_Ratio_clean.csv"),
  pension_expenditure = read_csv("R/Pension_as_percent_of_GDP_clean.csv"),
  social_security = read_csv("R/Social_Security_Contributions_clean.csv"),
  urban_population = read_csv("R/Urban_Population_Rate_clean.csv"),
  pension_gap = read_csv("R/Pension_financing_gap_clean.csv")
)

print("All datasets loaded successfully")

# Create master dataset by merging all datasets
print("Creating master dataset...")
master_data <- datasets$contraceptive %>%
  full_join(datasets$female_labor, by = c("Country", "Year")) %>%
  full_join(datasets$female_education, by = c("Country", "Year")) %>%
  full_join(datasets$fertility, by = c("Country", "Year")) %>%
  full_join(datasets$gdp, by = c("Country", "Year")) %>%
  full_join(datasets$life_expectancy, by = c("Country", "Year")) %>%
  full_join(datasets$old_age_dependency, by = c("Country", "Year")) %>%
  full_join(datasets$pension_expenditure, by = c("Country", "Year")) %>%
  full_join(datasets$social_security, by = c("Country", "Year")) %>%
  full_join(datasets$urban_population, by = c("Country", "Year")) %>%
  full_join(datasets$pension_gap, by = c("Country", "Year")) %>%
  # Add country classification
  mutate(
    Country_Group = case_when(
      Country %in% developed_countries ~ "Developed",
      Country %in% developing_countries ~ "Developing",
      TRUE ~ "Other"
    )
  ) %>%
  # Filter to analysis period (excluding incomplete 2022-2024 data)
  filter(Year >= 1990 & Year <= 2021) %>%
  arrange(Country, Year)

print(paste("Master dataset created with", nrow(master_data), "observations"))
print(paste("Countries included:", paste(unique(master_data$Country), collapse = ", ")))

# Function to create summary statistics
create_summary_stats <- function(data, group_name = "All Countries") {
  
  # Select numeric variables (excluding Year)
  numeric_vars <- data %>%
    select(-Country, -Year, -Country_Group) %>%
    select_if(is.numeric)
  
  # Calculate summary statistics
  stats <- numeric_vars %>%
    summarise_all(list(
      mean = ~round(mean(.x, na.rm = TRUE), 3),
      sd = ~round(sd(.x, na.rm = TRUE), 3),
      min = ~round(min(.x, na.rm = TRUE), 3),
      max = ~round(max(.x, na.rm = TRUE), 3),
      obs = ~sum(!is.na(.x))
    )) %>%
    # Reshape to long format
    pivot_longer(everything(), names_to = "variable_stat", values_to = "value") %>%
    # Separate variable name and statistic
    separate(variable_stat, into = c("variable", "statistic"), sep = "_(?=[^_]*$)") %>%
    # Reshape to wide format with statistics as columns
    pivot_wider(names_from = statistic, values_from = value) %>%
    # Reorder columns
    select(variable, obs, mean, sd, min, max) %>%
    # Clean up variable names for presentation
    mutate(
      Variable = case_when(
        variable == "CPR" ~ "Contraceptive Prevalence Rate (%)",
        variable == "FLFP" ~ "Female Labor Force Participation (%)",
        variable == "Female_tertiary_education" ~ "Female Tertiary Education Rate (%)",
        variable == "TFR" ~ "Total Fertility Rate",
        variable == "GDP_per_capita" ~ "GDP per Capita (2017 PPP $)",
        variable == "Life_expectancy_65" ~ "Life Expectancy at 65 (years)",
        variable == "Old_age_dependency" ~ "Old Age Dependency Ratio (%)",
        variable == "Pension_GDP" ~ "Pension Expenditure (% GDP)",
        variable == "Social_security_GDP" ~ "Social Security Contributions (% GDP)",
        variable == "Urban_rate" ~ "Urban Population Rate (%)",
        variable == "Pension_financing_gap" ~ "Pension Financing Gap (% GDP)",
        TRUE ~ variable
      )
    ) %>%
    select(Variable, obs, mean, sd, min, max) %>%
    # Rename columns for presentation
    rename(
      `Observations` = obs,
      `Mean` = mean,
      `Std. Dev.` = sd,
      `Min` = min,
      `Max` = max
    )
  
  return(stats)
}

# Create summary statistics tables
print("Creating summary statistics tables...")

# Overall summary statistics
overall_stats <- create_summary_stats(master_data, "All Countries")

# Developed countries
developed_stats <- master_data %>%
  filter(Country_Group == "Developed") %>%
  create_summary_stats("Developed Countries")

# Developing countries  
developing_stats <- master_data %>%
  filter(Country_Group == "Developing") %>%
  create_summary_stats("Developing Countries")

# Print tables
print("\n================================================================================")
print("SUMMARY STATISTICS - ALL COUNTRIES")
print("================================================================================")
print(overall_stats)

print("\n================================================================================")
print("SUMMARY STATISTICS - DEVELOPED COUNTRIES")
print("================================================================================")
print(developed_stats)

print("\n================================================================================")
print("SUMMARY STATISTICS - DEVELOPING COUNTRIES") 
print("================================================================================")
print(developing_stats)

# Save summary statistics to CSV files for easy import into LaTeX
write_csv(overall_stats, "R/summary_stats_all.csv")
write_csv(developed_stats, "R/summary_stats_developed.csv")
write_csv(developing_stats, "R/summary_stats_developing.csv")

# Save master dataset
write_csv(master_data, "R/master_dataset.csv")

print("\n✓ Summary statistics created and saved:")
print("  - R/summary_stats_all.csv")
print("  - R/summary_stats_developed.csv") 
print("  - R/summary_stats_developing.csv")
print("  - R/master_dataset.csv")

# Additional data coverage information
print("\n================================================================================")
print("DATA COVERAGE SUMMARY")
print("================================================================================")

coverage_summary <- master_data %>%
  group_by(Country, Country_Group) %>%
  summarise(
    Years_Available = n(),
    Year_Range = paste0(min(Year), "-", max(Year)),
    Missing_Variables = sum(is.na(CPR) + is.na(FLFP) + is.na(Female_tertiary_education) + 
                          is.na(TFR) + is.na(GDP_per_capita) + is.na(Life_expectancy_65) +
                          is.na(Old_age_dependency) + is.na(Pension_GDP) + 
                          is.na(Social_security_GDP) + is.na(Urban_rate) + is.na(Pension_financing_gap)),
    .groups = 'drop'
  ) %>%
  arrange(Country_Group, Country)

print(coverage_summary)

print(paste("\nTotal observations in master dataset:", nrow(master_data)))
print(paste("Countries:", length(unique(master_data$Country))))
print(paste("Years covered:", min(master_data$Year), "to", max(master_data$Year)))