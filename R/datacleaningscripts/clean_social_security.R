# Clean Social Security Contributions Data
# File: datasets/Social Security Contributions.csv
# Format: OECD format with different measures

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
print("Reading Social Security Contributions.csv...")
ssc_raw <- read_csv("datasets/Social Security Contributions.csv")

print("File structure:")
print(colnames(ssc_raw))

print("Unique countries in dataset:")
print(sort(unique(ssc_raw$Country)))

print("Available measures:")
print(unique(ssc_raw$MEASURE))

print("Countries from our analysis found in dataset:")
found_countries <- ssc_raw$Country[ssc_raw$Country %in% c(selected_countries, "Korea")]
print(unique(found_countries))

# Clean the data - focus on % of GDP measure
ssc_clean <- ssc_raw %>%
  filter(MEASURE == "PC_GDP") %>%  # Keep only % of GDP measure
  select(Country, TIME_PERIOD, OBS_VALUE) %>%
  rename(Year = TIME_PERIOD, Social_security_GDP = OBS_VALUE) %>%
  mutate(Country = standardize_country_names(Country)) %>%
  filter(Country %in% selected_countries) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(Social_security_GDP), !is.na(Year)) %>%
  filter(Year >= 1990 & Year <= 2024) %>%  # Restrict to analysis period
  arrange(Country, Year)

print("Cleaned data summary:")
print("Countries found:")
print(unique(ssc_clean$Country))

print("Year range:")
if(nrow(ssc_clean) > 0) {
  print(paste("From", min(ssc_clean$Year), "to", max(ssc_clean$Year)))
} else {
  print("No data found after cleaning")
}

print("Data availability by country:")
if(nrow(ssc_clean) > 0) {
  country_summary <- ssc_clean %>%
    group_by(Country) %>%
    summarise(
      observations = n(),
      min_year = min(Year),
      max_year = max(Year),
      min_ssc = round(min(Social_security_GDP, na.rm = TRUE), 2),
      max_ssc = round(max(Social_security_GDP, na.rm = TRUE), 2),
      .groups = 'drop'
    )
  print(country_summary)
} else {
  print("No data to summarize")
}

# Save cleaned data
write_csv(ssc_clean, "R/Social_Security_Contributions_clean.csv")

print("✓ Social security contributions data cleaned and saved to R/Social_Security_Contributions_clean.csv")
print(paste("Final dataset:", nrow(ssc_clean), "observations across", length(unique(ssc_clean$Country)), "countries"))