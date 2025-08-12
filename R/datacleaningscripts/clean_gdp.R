# Clean GDP per capita Data
# File: datasets/GDP per capita (constant 2015 US$).csv
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
print("Reading GDP per capita (constant 2015 US$).csv...")
gdp_raw <- read_csv("datasets/GDP per capita (constant 2015 US$).csv", skip = 4)

print("File structure:")
print(colnames(gdp_raw)[1:10])  # Show first 10 columns
print(paste("Total columns:", ncol(gdp_raw)))

print("Countries in our analysis found in dataset:")
found_countries <- gdp_raw$`Country Name`[gdp_raw$`Country Name` %in% c(selected_countries, "Korea, Rep.")]
print(found_countries)

# Check column names before cleaning
print("All column names after removing metadata columns:")
year_columns <- names(select(gdp_raw, -`Country Code`, -`Indicator Name`, -`Indicator Code`, -`Country Name`))
print(year_columns)

print("Column names that will cause coercion warnings (non-numeric):")
problematic_cols <- year_columns[is.na(as.numeric(year_columns))]
print(problematic_cols)

# Clean the data
gdp_temp <- gdp_raw %>%
  select(-`Country Code`, -`Indicator Name`, -`Indicator Code`) %>%
  rename(Country = `Country Name`) %>%
  mutate(Country = standardize_country_names(Country)) %>%
  filter(Country %in% selected_countries)

print(paste("Data before pivot_longer: ", nrow(gdp_temp), "rows,", ncol(gdp_temp), "columns"))

gdp_clean <- gdp_temp %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "GDP_per_capita") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(GDP_per_capita), !is.na(Year)) %>%  # This removes rows where Year conversion failed
  filter(Year >= 1990 & Year <= 2024) %>%  # Restrict to analysis period
  arrange(Country, Year)

print("Cleaned data summary:")
print("Countries found:")
print(unique(gdp_clean$Country))

print("Year range:")
print(paste("From", min(gdp_clean$Year), "to", max(gdp_clean$Year)))

print("Data availability by country:")
country_summary <- gdp_clean %>%
  group_by(Country) %>%
  summarise(
    observations = n(),
    min_year = min(Year),
    max_year = max(Year),
    min_gdp = round(min(GDP_per_capita, na.rm = TRUE), 0),
    max_gdp = round(max(GDP_per_capita, na.rm = TRUE), 0),
    .groups = 'drop'
  )
print(country_summary)

# Save cleaned data
write_csv(gdp_clean, "R/GDP_per_capita_clean.csv")

print("✓ GDP per capita data cleaned and saved to R/GDP_per_capita_clean.csv")
print(paste("Final dataset:", nrow(gdp_clean), "observations across", length(unique(gdp_clean$Country)), "countries"))