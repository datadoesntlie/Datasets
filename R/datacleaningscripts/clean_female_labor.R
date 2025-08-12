# Clean Female Labor Force Participation Rate Data
# File: datasets/Female labor force participation rate.csv
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
print("Reading Female labor force participation rate.csv...")
flfp_raw <- read_csv("datasets/Female labor force participation rate.csv", skip = 4)

print("File structure:")
print(colnames(flfp_raw)[1:10])  # Show first 10 columns
print(paste("Total columns:", ncol(flfp_raw)))

print("First few rows (country names):")
print(head(flfp_raw$`Country Name`, 10))

print("Unique countries in dataset:")
print(paste("Number of countries:", length(unique(flfp_raw$`Country Name`))))

# Clean the data
flfp_clean <- flfp_raw %>%
  select(-`Country Code`, -`Indicator Name`, -`Indicator Code`) %>%
  rename(Country = `Country Name`) %>%
  mutate(Country = standardize_country_names(Country)) %>%
  filter(Country %in% selected_countries) %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "FLFP") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(FLFP), !is.na(Year)) %>%  # This removes rows where Year conversion failed
  filter(Year >= 1990 & Year <= 2024) %>%  # Restrict to analysis period
  arrange(Country, Year)

print("Cleaned data summary:")
print("Countries found:")
print(unique(flfp_clean$Country))

print("Year range:")
print(paste("From", min(flfp_clean$Year), "to", max(flfp_clean$Year)))

print("Data availability by country:")
country_summary <- flfp_clean %>%
  group_by(Country) %>%
  summarise(
    observations = n(),
    min_year = min(Year),
    max_year = max(Year),
    min_flfp = round(min(FLFP, na.rm = TRUE), 1),
    max_flfp = round(max(FLFP, na.rm = TRUE), 1),
    .groups = 'drop'
  )
print(country_summary)

# Save cleaned data
write_csv(flfp_clean, "R/Female_labor_force_participation_rate_clean.csv")

print("✓ Female labor force participation rate data cleaned and saved to R/Female_labor_force_participation_rate_clean.csv")
print(paste("Final dataset:", nrow(flfp_clean), "observations across", length(unique(flfp_clean$Country)), "countries"))