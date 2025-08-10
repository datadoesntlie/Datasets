# Clean Total Fertility Rate Data (World Bank)
# File: datasets/Total_Fertility_Rate_WorldBank.csv
# Format: CSV format extracted from World Bank xlsx file

library(dplyr)
library(readr)

# Define the 11 countries for analysis
selected_countries <- c("Greece", "Japan", "Italy", "Spain", "Germany", "France", 
                       "Sweden", "South Korea", "Brazil", "Mexico", "Chile")

# Country name standardization mapping
country_mapping <- c(
  "Korea, Rep." = "South Korea",
  "Korea" = "South Korea",
  "Republic of Korea" = "South Korea",
  "Greece" = "Greece",
  "Japan" = "Japan", 
  "Italy" = "Italy",
  "Spain" = "Spain",
  "Germany" = "Germany",
  "France" = "France",
  "Sweden" = "Sweden",
  "Brazil" = "Brazil",
  "Mexico" = "Mexico",
  "Chile" = "Chile"
)

# Function to standardize country names
standardize_country_names <- function(country_vector) {
  ifelse(country_vector %in% names(country_mapping),
         country_mapping[country_vector],
         country_vector)
}

# Read and examine the CSV file structure
print("Reading Total_Fertility_Rate_WorldBank.csv...")

# Read the CSV file
tfr_raw <- read_csv("datasets/ Total_Fertility_Rate_WorldBank.csv")

print("File structure:")
print(colnames(tfr_raw))
print(paste("Total columns:", ncol(tfr_raw)))
print(paste("Total rows:", nrow(tfr_raw)))

print("First few rows:")
print(head(tfr_raw, 3))

# Identify potential country column
potential_country_cols <- colnames(tfr_raw)[grepl("country|Country|COUNTRY|name|Name", colnames(tfr_raw), ignore.case = TRUE)]
print("Potential country columns:")
print(potential_country_cols)

if(length(potential_country_cols) > 0) {
  country_col <- potential_country_cols[1]
  print(paste("Using country column:", country_col))
  print("Countries from our analysis found in dataset:")
  found_countries <- tfr_raw[[country_col]][tfr_raw[[country_col]] %in% c(selected_countries, "Korea, Rep.")]
  print(unique(found_countries))
} else {
  print("No obvious country column found.")
}

# Look for year columns (assume World Bank wide format)
year_cols <- colnames(tfr_raw)[grepl("^[0-9]{4}$", colnames(tfr_raw))]
print("Year columns found:")
print(head(year_cols, 10))  # Show first 10 year columns

# Clean the data (this is already in long format)
print("Cleaning World Bank long format data...")

tfr_clean <- tfr_raw %>%
  select(Country, Year, `Value Numeric`) %>%
  rename(TFR_WorldBank = `Value Numeric`) %>%
  mutate(Country = standardize_country_names(Country)) %>%
  filter(Country %in% selected_countries) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(TFR_WorldBank), !is.na(Year)) %>%
  filter(Year >= 1990 & Year <= 2024) %>%  # Restrict to analysis period
  arrange(Country, Year)

print("Cleaned data summary:")
print("Countries found:")
print(unique(tfr_clean$Country))

if(nrow(tfr_clean) > 0) {
  print("Year range:")
  print(paste("From", min(tfr_clean$Year), "to", max(tfr_clean$Year)))
  
  print("Data availability by country:")
  country_summary <- tfr_clean %>%
    group_by(Country) %>%
    summarise(
      observations = n(),
      min_year = min(Year),
      max_year = max(Year),
      min_tfr = round(min(TFR_WorldBank, na.rm = TRUE), 2),
      max_tfr = round(max(TFR_WorldBank, na.rm = TRUE), 2),
      .groups = 'drop'
    )
  print(country_summary)
} else {
  print("No data found after cleaning")
}

# Save cleaned data
write_csv(tfr_clean, "R/Total_Fertility_Rate_WorldBank_clean.csv")

print("✓ Total fertility rate (World Bank) data cleaned and saved to R/Total_Fertility_Rate_WorldBank_clean.csv")
print(paste("Final dataset:", nrow(tfr_clean), "observations across", length(unique(tfr_clean$Country)), "countries"))