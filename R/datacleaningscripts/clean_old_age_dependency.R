# Clean Old Age Dependency Ratio Data
# File: datasets/Old Age Dependancy Ratio.csv
# Format: OECD format

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
print("Reading Old Age Dependancy Ratio.csv...")
old_age_raw <- read_csv("datasets/Old Age Dependancy Ratio.csv")

print("File structure:")
print(colnames(old_age_raw))

print("Unique countries in dataset:")
print(sort(unique(old_age_raw$Country)))

print("Countries from our analysis found in dataset:")
found_countries <- old_age_raw$Country[old_age_raw$Country %in% c(selected_countries, "Korea")]
print(unique(found_countries))

# Clean the data
old_age_clean <- old_age_raw %>%
  select(Country, TIME_PERIOD, OBS_VALUE) %>%
  rename(Year = TIME_PERIOD, Old_age_dependency = OBS_VALUE) %>%
  mutate(Country = standardize_country_names(Country)) %>%
  filter(Country %in% selected_countries) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(Old_age_dependency), !is.na(Year)) %>%
  filter(Year >= 1990 & Year <= 2024) %>%  # Restrict to analysis period
  arrange(Country, Year)

print("Cleaned data summary:")
print("Countries found:")
print(unique(old_age_clean$Country))

print("Year range:")
if(nrow(old_age_clean) > 0) {
  print(paste("From", min(old_age_clean$Year), "to", max(old_age_clean$Year)))
} else {
  print("No data found after cleaning")
}

print("Data availability by country:")
if(nrow(old_age_clean) > 0) {
  country_summary <- old_age_clean %>%
    group_by(Country) %>%
    summarise(
      observations = n(),
      min_year = min(Year),
      max_year = max(Year),
      min_dependency = round(min(Old_age_dependency, na.rm = TRUE), 1),
      max_dependency = round(max(Old_age_dependency, na.rm = TRUE), 1),
      .groups = 'drop'
    )
  print(country_summary)
} else {
  print("No data to summarize")
}

# Save cleaned data
write_csv(old_age_clean, "R/Old_Age_Dependancy_Ratio_clean.csv")

print("✓ Old age dependency ratio data cleaned and saved to R/Old_Age_Dependancy_Ratio_clean.csv")
print(paste("Final dataset:", nrow(old_age_clean), "observations across", length(unique(old_age_clean$Country)), "countries"))