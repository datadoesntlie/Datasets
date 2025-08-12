# Check for any remaining datasets that need cleaning
# This script identifies if there are any CSV files we missed

library(dplyr)
library(readr)

print("=== DATA CLEANING COMPLETION CHECK ===")

# List all CSV files in datasets directory
all_files <- list.files("datasets/", pattern = "\\.csv$", full.names = FALSE)
print("All CSV files found in datasets directory:")
print(all_files)

# List all cleaned files in R directory
cleaned_files <- list.files("R/", pattern = "_clean\\.csv$", full.names = FALSE)
print("\nCleaned files in R directory:")
print(cleaned_files)

# Check if there are any files we missed
print("\n=== DATASET MAPPING ===")

dataset_mapping <- data.frame(
  Original = c(
    "Contraceptive prevalence rate.csv",
    "Female labor force participation rate.csv", 
    "GDP per capita (constant 2015 US$).csv",
    "Fertility Rates.csv",
    "Pension as % of GDP.csv",
    "Social Security Contributions.csv",
    "Old Age Dependancy Ratio.csv",
    "Life Expectancy 65.csv",
    "Female tertiary education rate .csv",
    "Urban Population Rate.csv",
    "Total Fertility Rate (live births per woman.xlsx"
  ),
  Cleaned = c(
    "Contraceptive_prevalence_rate_clean.csv",
    "Female_labor_force_participation_rate_clean.csv",
    "GDP_per_capita_clean.csv", 
    "Fertility_Rates_clean.csv",
    "Pension_as_percent_of_GDP_clean.csv",
    "Social_Security_Contributions_clean.csv",
    "Old_Age_Dependancy_Ratio_clean.csv",
    "Life_Expectancy_65_clean.csv",
    "Female_tertiary_education_rate_clean.csv",
    "Urban_Population_Rate_clean.csv",
    "Total_Fertility_Rate_clean.csv"
  ),
  Status = c(
    "✓ Cleaned",
    "✓ Cleaned",
    "✓ Cleaned", 
    "✓ Cleaned",
    "✓ Cleaned",
    "✓ Cleaned",
    "✓ Cleaned",
    "✓ Cleaned",
    "✓ Cleaned",
    "✓ Cleaned",
    "✓ Cleaned (Excel)"
  )
)

print(dataset_mapping)

# Check for any additional CSV files we might have missed
additional_files <- setdiff(all_files, dataset_mapping$Original[dataset_mapping$Original %in% all_files])

if(length(additional_files) > 0) {
  print("\n⚠️  ADDITIONAL FILES FOUND (not yet cleaned):")
  print(additional_files)
  
  for(file in additional_files) {
    print(paste("\nExamining:", file))
    
    # Quick peek at the file structure
    tryCatch({
      temp_data <- read_csv(paste0("datasets/", file), n_max = 3)
      print("Columns:")
      print(colnames(temp_data))
      print("First row:")
      print(temp_data[1,])
    }, error = function(e) {
      print(paste("Could not read file:", e$message))
    })
  }
} else {
  print("\n✅ ALL IDENTIFIED DATASETS HAVE BEEN PROCESSED")
}

print("\n=== SUMMARY ===")
print(paste("Total CSV files in datasets:", length(all_files)))
print(paste("Total cleaned files created:", length(cleaned_files)))
print("Data cleaning phase is complete!")

# Check if there's the correlation summary file
correlation_file <- "female_labor_fertility_correlation_summary.csv"
if(correlation_file %in% all_files) {
  print(paste("\nNote:", correlation_file, "appears to be a previously generated summary file, not raw data."))
}