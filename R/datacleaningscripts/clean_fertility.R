# Clean Fertility Rates Data
# File: datasets/Fertility Rates.csv
# Format: OECD format (long format)

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
print("Reading Fertility Rates.csv...")
fertility_raw <- read_csv("datasets/Fertility Rates.csv")

print("File structure:")
print(colnames(fertility_raw))

print("Unique countries in dataset:")
print(sort(unique(fertility_raw$Country)))

print("Countries from our analysis found in dataset:")
found_countries <- fertility_raw$Country[fertility_raw$Country %in% c(selected_countries, "Korea")]
print(unique(found_countries))

# Clean the data
fertility_clean <- fertility_raw %>%
  select(Country, TIME_PERIOD, OBS_VALUE) %>%
  rename(Year = TIME_PERIOD, TFR = OBS_VALUE) %>%
  mutate(Country = standardize_country_names(Country)) %>%
  filter(Country %in% selected_countries) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(TFR), !is.na(Year)) %>%
  filter(Year >= 1990 & Year <= 2024) %>%  # Restrict to analysis period
  arrange(Country, Year)

print("Cleaned data summary:")
print("Countries found:")
print(unique(fertility_clean$Country))

print("Year range:")
print(paste("From", min(fertility_clean$Year), "to", max(fertility_clean$Year)))

print("Data availability by country:")
country_summary <- fertility_clean %>%
  group_by(Country) %>%
  summarise(
    observations = n(),
    min_year = min(Year),
    max_year = max(Year),
    min_tfr = round(min(TFR, na.rm = TRUE), 2),
    max_tfr = round(max(TFR, na.rm = TRUE), 2),
    .groups = 'drop'
  )
print(country_summary)

# Save cleaned data
write_csv(fertility_clean, "R/Fertility_Rates_clean.csv")

print("✓ Fertility rates data cleaned and saved to R/Fertility_Rates_clean.csv")
print(paste("Final dataset:", nrow(fertility_clean), "observations across", length(unique(fertility_clean$Country)), "countries"))