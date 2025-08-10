# Clean Educational Attainment Data
# File: datasets/Educational attainment by level of education, cumulative (% population 25+).csv
# Format: World Bank format (long format)

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
print("Reading Educational attainment by level of education, cumulative (% population 25+).csv...")
edu_raw <- read_csv("datasets/Educational attainment by level of education, cumulative (% population 25+).csv")

print("File structure:")
print(colnames(edu_raw))

print("Unique countries in dataset:")
print(paste("Total countries:", length(unique(edu_raw$`Country Name`))))

print("Available education indicators:")
print(unique(edu_raw$`Indicator Name`))

print("Available disaggregations:")
print(unique(edu_raw$Disaggregation))

print("Countries from our analysis found in dataset:")
found_countries <- edu_raw$`Country Name`[edu_raw$`Country Name` %in% c(selected_countries, "Korea, Rep.")]
print(unique(found_countries))

# Focus on tertiary education indicators that are most relevant
# We'll prioritize "at least Bachelor's or equivalent" as it's most comparable to our other tertiary education data
tertiary_indicators <- c(
  "at least Bachelor's or equivalent",
  "at least completed short-cycle tertiary", 
  "at least completed post-secondary"
)

print("Filtering for tertiary education levels...")
print("Using indicators:", tertiary_indicators)

# Clean the data - focus on female tertiary education
edu_clean <- edu_raw %>%
  filter(Disaggregation %in% paste0(tertiary_indicators, ", female")) %>%
  select(`Country Name`, Year, Value, Disaggregation) %>%
  rename(Country = `Country Name`, Educational_attainment = Value) %>%
  mutate(Country = standardize_country_names(Country)) %>%
  filter(Country %in% selected_countries) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(Educational_attainment), !is.na(Year)) %>%
  filter(Year >= 1990 & Year <= 2024) %>%  # Restrict to analysis period
  # Prioritize Bachelor's level, then short-cycle tertiary, then post-secondary
  mutate(
    Priority = case_when(
      grepl("Bachelor", Disaggregation) ~ 1,
      grepl("short-cycle tertiary", Disaggregation) ~ 2,
      grepl("post-secondary", Disaggregation) ~ 3,
      TRUE ~ 4
    )
  ) %>%
  group_by(Country, Year) %>%
  filter(Priority == min(Priority)) %>%  # Keep highest priority indicator per country-year
  ungroup() %>%
  select(-Priority, -Disaggregation) %>%
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
      min_education = round(min(Educational_attainment, na.rm = TRUE), 1),
      max_education = round(max(Educational_attainment, na.rm = TRUE), 1),
      .groups = 'drop'
    )
  print(country_summary)
} else {
  print("No data to summarize")
}

# Save cleaned data
write_csv(edu_clean, "R/Educational_attainment_cumulative_clean.csv")

print("✓ Educational attainment data cleaned and saved to R/Educational_attainment_cumulative_clean.csv")
print(paste("Final dataset:", nrow(edu_clean), "observations across", length(unique(edu_clean$Country)), "countries"))

# Note about the data
print("\nNote: This dataset focuses on tertiary education levels (Bachelor's or equivalent)")
print("and may complement the existing Female_tertiary_education_rate_clean.csv file.")
print("Consider using whichever has better coverage for your analysis period.")