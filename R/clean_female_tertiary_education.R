# Clean Female Tertiary Education Rate Data
# File: datasets/Female tertiary education rate .csv
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
print("Reading Female tertiary education rate .csv...")
edu_raw <- read_csv("datasets/Female tertiary education rate .csv")

print("File structure:")
print(colnames(edu_raw))

print("Unique countries in dataset:")
print(sort(unique(edu_raw$Country)))

print("Countries from our analysis found in dataset:")
found_countries <- edu_raw$Country[edu_raw$Country %in% c(selected_countries, "Korea")]
print(unique(found_countries))

# Clean the data
edu_clean <- edu_raw %>%
  select(Country, TIME_PERIOD, OBS_VALUE) %>%
  rename(Year = TIME_PERIOD, Female_tertiary_education = OBS_VALUE) %>%
  mutate(Country = standardize_country_names(Country)) %>%
  filter(Country %in% selected_countries) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(Female_tertiary_education), !is.na(Year)) %>%
  filter(Year >= 1990 & Year <= 2024) %>%  # Restrict to analysis period
  arrange(Country, Year)

print("Cleaned data summary:")
print("Countries found:")
print(unique(edu_clean$Country))

print("Year range:")
if(nrow(edu_clean) > 0) {
  print(paste("From", min(edu_clean$Year), "to", max(edu_clean$Year)))
} else {
  print("No data found after cleaning")
}

print("Data availability by country:")
if(nrow(edu_clean) > 0) {
  country_summary <- edu_clean %>%
    group_by(Country) %>%
    summarise(
      observations = n(),
      min_year = min(Year),
      max_year = max(Year),
      min_education = round(min(Female_tertiary_education, na.rm = TRUE), 1),
      max_education = round(max(Female_tertiary_education, na.rm = TRUE), 1),
      .groups = 'drop'
    )
  print(country_summary)
} else {
  print("No data to summarize")
}

# Save cleaned data
write_csv(edu_clean, "R/Female_tertiary_education_rate_clean.csv")

print("✓ Female tertiary education data cleaned and saved to R/Female_tertiary_education_rate_clean.csv")
print(paste("Final dataset:", nrow(edu_clean), "observations across", length(unique(edu_clean$Country)), "countries"))