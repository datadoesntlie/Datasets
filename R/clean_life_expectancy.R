# Clean Life Expectancy 65 Data
# File: datasets/Life Expectancy 65.csv
# Format: OECD Health Statistics format

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
print("Reading Life Expectancy 65.csv...")
life_exp_raw <- read_csv("datasets/Life Expectancy 65.csv")

print("File structure:")
print(colnames(life_exp_raw))

print("Unique reference areas (countries) in dataset:")
print(sort(unique(life_exp_raw$`Reference area`)))

print("Countries from our analysis found in dataset:")
found_countries <- life_exp_raw$`Reference area`[life_exp_raw$`Reference area` %in% c(selected_countries, "Korea")]
print(unique(found_countries))

print("Sample of data structure:")
print(head(life_exp_raw %>% select(`Reference area`, TIME_PERIOD, OBS_VALUE), 5))

# Check gender information in the data
print("Gender information in dataset:")
print(unique(life_exp_raw$Sex))

# Clean the data - average male and female life expectancy
life_exp_clean <- life_exp_raw %>%
  select(`Reference area`, TIME_PERIOD, OBS_VALUE, Sex) %>%
  rename(Country = `Reference area`, Year = TIME_PERIOD, Life_expectancy_65 = OBS_VALUE) %>%
  mutate(Country = standardize_country_names(Country)) %>%
  filter(Country %in% selected_countries) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(Life_expectancy_65), !is.na(Year)) %>%
  filter(Year >= 1990 & Year <= 2024) %>%  # Restrict to analysis period
  # Average male and female life expectancy by country-year
  group_by(Country, Year) %>%
  summarise(
    Life_expectancy_65 = mean(Life_expectancy_65, na.rm = TRUE),
    gender_count = n(),
    .groups = 'drop'
  ) %>%
  # Only keep observations where we have data (should be 1 or 2 genders per country-year)
  filter(gender_count > 0) %>%
  select(-gender_count) %>%
  arrange(Country, Year)

print("Cleaned data summary:")
print("Countries found:")
print(unique(life_exp_clean$Country))

print("Year range:")
if(nrow(life_exp_clean) > 0) {
  print(paste("From", min(life_exp_clean$Year), "to", max(life_exp_clean$Year)))
} else {
  print("No data found after cleaning")
}

print("Data availability by country:")
if(nrow(life_exp_clean) > 0) {
  country_summary <- life_exp_clean %>%
    group_by(Country) %>%
    summarise(
      observations = n(),
      min_year = min(Year),
      max_year = max(Year),
      min_life_exp = round(min(Life_expectancy_65, na.rm = TRUE), 1),
      max_life_exp = round(max(Life_expectancy_65, na.rm = TRUE), 1),
      .groups = 'drop'
    )
  print(country_summary)
} else {
  print("No data to summarize")
}

# Save cleaned data
write_csv(life_exp_clean, "R/Life_Expectancy_65_clean.csv")

print("✓ Life expectancy data cleaned and saved to R/Life_Expectancy_65_clean.csv")
print(paste("Final dataset:", nrow(life_exp_clean), "observations across", length(unique(life_exp_clean$Country)), "countries"))