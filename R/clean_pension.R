# Clean Pension as % of GDP Data
# File: datasets/Pension as % of GDP.csv
# Format: OECD SOCX format (complex structure)

library(dplyr)
library(readr)

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
print("Reading Pension as % of GDP.csv...")
pension_raw <- read_csv("datasets/Pension as % of GDP.csv")

print("File structure:")
print(colnames(pension_raw))

print("Unique reference areas (countries) in dataset:")
print(sort(unique(pension_raw$`Reference area`)))

print("Countries from our analysis found in dataset:")
found_countries <- pension_raw$`Reference area`[pension_raw$`Reference area` %in% c(selected_countries, "Korea")]
print(unique(found_countries))

print("Sample of data structure:")
print(head(pension_raw %>% select(`Reference area`, TIME_PERIOD, OBS_VALUE), 5))

# Clean the data
pension_clean <- pension_raw %>%
  select(`Reference area`, TIME_PERIOD, OBS_VALUE) %>%
  rename(Country = `Reference area`, Year = TIME_PERIOD, Pension_GDP = OBS_VALUE) %>%
  mutate(Country = standardize_country_names(Country)) %>%
  filter(Country %in% selected_countries) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(Pension_GDP), !is.na(Year)) %>%
  filter(Year >= 1990 & Year <= 2024) %>%  # Restrict to analysis period
  arrange(Country, Year)

print("Cleaned data summary:")
print("Countries found:")
print(unique(pension_clean$Country))

print("Year range:")
if(nrow(pension_clean) > 0) {
  print(paste("From", min(pension_clean$Year), "to", max(pension_clean$Year)))
} else {
  print("No data found after cleaning")
}

print("Data availability by country:")
if(nrow(pension_clean) > 0) {
  country_summary <- pension_clean %>%
    group_by(Country) %>%
    summarise(
      observations = n(),
      min_year = min(Year),
      max_year = max(Year),
      min_pension = round(min(Pension_GDP, na.rm = TRUE), 2),
      max_pension = round(max(Pension_GDP, na.rm = TRUE), 2),
      .groups = 'drop'
    )
  print(country_summary)
} else {
  print("No data to summarize")
}

# Save cleaned data
write_csv(pension_clean, "R/Pension_as_percent_of_GDP_clean.csv")

print("✓ Pension expenditure data cleaned and saved to R/Pension_as_percent_of_GDP_clean.csv")
print(paste("Final dataset:", nrow(pension_clean), "observations across", length(unique(pension_clean$Country)), "countries"))