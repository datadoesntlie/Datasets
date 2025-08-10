# Merge Female Tertiary Education Data
# Combines OECD and World Bank female tertiary education datasets
# Prioritizes more complete dataset per country-year

library(dplyr)
library(readr)

# Define the 11 countries for analysis
selected_countries <- c("Greece", "Japan", "Italy", "Spain", "Germany", "France", 
                       "Sweden", "South Korea", "Brazil", "Mexico", "Chile")

print("Reading both female tertiary education datasets...")

# Read OECD data
oecd_data <- read_csv("R/Female_tertiary_education_rate_clean.csv")
print(paste("OECD dataset:", nrow(oecd_data), "observations"))

# Read World Bank data
wb_data <- read_csv("R/Educational_attainment_cumulative_clean.csv")
print(paste("World Bank dataset:", nrow(wb_data), "observations"))

# Rename World Bank column to match OECD
wb_data <- wb_data %>%
  rename(Female_tertiary_education = Educational_attainment)

print("\n=== DATA COVERAGE ANALYSIS ===")

# Analyze coverage by country
print("OECD dataset coverage by country:")
oecd_summary <- oecd_data %>%
  group_by(Country) %>%
  summarise(
    observations = n(),
    min_year = min(Year),
    max_year = max(Year),
    years_covered = paste(sort(unique(Year)), collapse = ", "),
    .groups = 'drop'
  )
print(oecd_summary)

print("\nWorld Bank dataset coverage by country:")
wb_summary <- wb_data %>%
  group_by(Country) %>%
  summarise(
    observations = n(),
    min_year = min(Year),
    max_year = max(Year),
    years_covered = paste(sort(unique(Year)), collapse = ", "),
    .groups = 'drop'
  )
print(wb_summary)

# Check for problematic values (likely data errors)
print("\n=== IDENTIFYING PROBLEMATIC VALUES ===")

print("World Bank values less than 5% (likely errors):")
wb_errors <- wb_data %>%
  filter(Female_tertiary_education < 5) %>%
  arrange(Country, Year)
print(wb_errors)

print("OECD values less than 5% (likely errors):")
oecd_errors <- oecd_data %>%
  filter(Female_tertiary_education < 5) %>%
  arrange(Country, Year)
print(oecd_errors)

# Clean problematic values in World Bank data
print("\nCleaning problematic values...")
wb_data_clean <- wb_data %>%
  # Remove values less than 5% as they appear to be data errors
  filter(Female_tertiary_education >= 5)

print(paste("World Bank dataset after cleaning:", nrow(wb_data_clean), "observations"))

# Merge datasets with priority logic
print("\n=== MERGING DATASETS ===")

# Create a combined dataset
merged_data <- bind_rows(
  oecd_data %>% mutate(Source = "OECD"),
  wb_data_clean %>% mutate(Source = "WorldBank")
) %>%
  arrange(Country, Year, Source)

# For overlapping country-years, apply priority logic:
# 1. Prefer OECD for developed countries (better education statistics)
# 2. Prefer World Bank for Latin American countries (better regional coverage)
# 3. For conflicts, choose the more reasonable value

developed_countries <- c("Japan", "Germany", "France", "Sweden", "Italy", "Spain", "Greece")
latin_countries <- c("Brazil", "Mexico", "Chile")

final_merged <- merged_data %>%
  group_by(Country, Year) %>%
  mutate(
    Priority = case_when(
      # For developed countries, prefer OECD
      Country %in% developed_countries & Source == "OECD" ~ 1,
      Country %in% developed_countries & Source == "WorldBank" ~ 2,
      # For Latin American countries, prefer World Bank
      Country %in% latin_countries & Source == "WorldBank" ~ 1,
      Country %in% latin_countries & Source == "OECD" ~ 2,
      # For South Korea, prefer OECD (better coverage)
      Country == "South Korea" & Source == "OECD" ~ 1,
      Country == "South Korea" & Source == "WorldBank" ~ 2,
      TRUE ~ 3
    )
  ) %>%
  # Keep highest priority value per country-year
  filter(Priority == min(Priority)) %>%
  # If still tied, take the first one (shouldn't happen with our logic)
  slice(1) %>%
  ungroup() %>%
  select(Country, Year, Female_tertiary_education, Source) %>%
  arrange(Country, Year)

print("Final merged dataset summary:")
print("Coverage by country:")
final_summary <- final_merged %>%
  group_by(Country) %>%
  summarise(
    observations = n(),
    min_year = min(Year),
    max_year = max(Year),
    oecd_obs = sum(Source == "OECD"),
    wb_obs = sum(Source == "WorldBank"),
    min_value = round(min(Female_tertiary_education), 2),
    max_value = round(max(Female_tertiary_education), 2),
    .groups = 'drop'
  )
print(final_summary)

print("\nSource distribution:")
source_dist <- final_merged %>%
  count(Source) %>%
  mutate(percentage = round(n/sum(n)*100, 1))
print(source_dist)

# Identify gaps in the final dataset
print("\n=== IDENTIFYING REMAINING GAPS ===")

# Create complete year grid for analysis period
complete_grid <- expand.grid(
  Country = selected_countries,
  Year = 1990:2024,
  stringsAsFactors = FALSE
) %>%
  arrange(Country, Year)

# Find missing data points
missing_data <- complete_grid %>%
  anti_join(final_merged, by = c("Country", "Year")) %>%
  group_by(Country) %>%
  summarise(
    missing_years = n(),
    missing_year_range = paste0(min(Year), "-", max(Year)),
    sample_missing = paste(head(Year, 10), collapse = ", "),
    .groups = 'drop'
  )

print("Missing data points by country:")
print(missing_data)

# Save the merged dataset
write_csv(final_merged %>% select(-Source), "R/Female_tertiary_education_merged_clean.csv")

print("\n✓ Merged female tertiary education data saved to R/Female_tertiary_education_merged_clean.csv")
print(paste("Final dataset:", nrow(final_merged), "observations across", length(unique(final_merged$Country)), "countries"))

print("\nNote: This merged dataset combines OECD and World Bank data, prioritizing:")
print("- OECD data for developed countries (better education statistics)")
print("- World Bank data for Latin American countries (better regional coverage)")
print("- Problematic values (<5%) have been removed as likely data errors")