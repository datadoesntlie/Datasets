# Clean Contraceptive Prevalence Rate Data
# File: datasets/Contraceptive prevalence rate.csv
# Format: UN Population Division format

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
print("Reading Contraceptive prevalence rate.csv...")
cpr_raw <- read_csv("datasets/Contraceptive prevalence rate.csv")

print("File structure:")
print(colnames(cpr_raw))
print("First few rows:")
print(head(cpr_raw, 3))

print("Unique countries in dataset:")
print(sort(unique(cpr_raw$Location)))

# Clean the data
cpr_clean <- cpr_raw %>%
  select(Location, Time, Value) %>%
  rename(Country = Location, Year = Time, CPR = Value) %>%
  mutate(Country = standardize_country_names(Country)) %>%
  filter(Country %in% selected_countries) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(CPR), !is.na(Year)) %>%
  filter(Year >= 1990 & Year <= 2024) %>%  # Restrict to analysis period
  arrange(Country, Year)

print("Cleaned data summary:")
print("Countries found:")
print(unique(cpr_clean$Country))

print("Year range:")
print(paste("From", min(cpr_clean$Year), "to", max(cpr_clean$Year)))

print("Data availability by country:")
country_summary <- cpr_clean %>%
  group_by(Country) %>%
  summarise(
    observations = n(),
    min_year = min(Year),
    max_year = max(Year),
    min_cpr = round(min(CPR, na.rm = TRUE), 1),
    max_cpr = round(max(CPR, na.rm = TRUE), 1),
    .groups = 'drop'
  )
print(country_summary)

# Save cleaned data
write_csv(cpr_clean, "R/Contraceptive_prevalence_rate_clean.csv")

print("✓ Contraceptive prevalence rate data cleaned and saved to R/Contraceptive_prevalence_rate_clean.csv")
print(paste("Final dataset:", nrow(cpr_clean), "observations across", length(unique(cpr_clean$Country)), "countries"))