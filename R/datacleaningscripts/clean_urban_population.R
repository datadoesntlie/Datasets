# Clean Urban Population Rate Data
# File: datasets/Urban Population Rate.csv
# Format: World Bank format (wide format with years as columns)

library(dplyr)
library(readr)
library(tidyr)

# Define the 11 countries for analysis
selected_countries <- c("Greece", "Japan", "Italy", "Spain", "Germany", "France", 
                       "Sweden", "South Korea", "Brazil", "Mexico", "Chile")

# Country name standardization mapping
country_mapping <- c(
  "Korea, Rep." = "South Korea",
  "Korea" = "South Korea",
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

# Read and examine the file structure
print("Reading Urban Population Rate.csv...")
urban_raw <- read_csv("datasets/Urban Population Rate.csv", skip = 4)

print("File structure:")
print(colnames(urban_raw)[1:10])  # Show first 10 columns
print(paste("Total columns:", ncol(urban_raw)))

print("Countries from our analysis found in dataset:")
found_countries <- urban_raw$`Country Name`[urban_raw$`Country Name` %in% c(selected_countries, "Korea, Rep.")]
print(found_countries)

# Clean the data
urban_clean <- urban_raw %>%
  select(-`Country Code`, -`Indicator Name`, -`Indicator Code`) %>%
  rename(Country = `Country Name`) %>%
  mutate(Country = standardize_country_names(Country)) %>%
  filter(Country %in% selected_countries) %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "Urban_rate") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(Urban_rate), !is.na(Year)) %>%  # This removes rows where Year conversion failed
  filter(Year >= 1990 & Year <= 2024) %>%  # Restrict to analysis period
  arrange(Country, Year)

print("Cleaned data summary:")
print("Countries found:")
print(unique(urban_clean$Country))

print("Year range:")
if(nrow(urban_clean) > 0) {
  print(paste("From", min(urban_clean$Year), "to", max(urban_clean$Year)))
} else {
  print("No data found after cleaning")
}

print("Data availability by country:")
if(nrow(urban_clean) > 0) {
  country_summary <- urban_clean %>%
    group_by(Country) %>%
    summarise(
      observations = n(),
      min_year = min(Year),
      max_year = max(Year),
      min_urban = round(min(Urban_rate, na.rm = TRUE), 1),
      max_urban = round(max(Urban_rate, na.rm = TRUE), 1),
      .groups = 'drop'
    )
  print(country_summary)
} else {
  print("No data to summarize")
}

# Save cleaned data
write_csv(urban_clean, "R/Urban_Population_Rate_clean.csv")

print("✓ Urban population rate data cleaned and saved to R/Urban_Population_Rate_clean.csv")
print(paste("Final dataset:", nrow(urban_clean), "observations across", length(unique(urban_clean$Country)), "countries"))