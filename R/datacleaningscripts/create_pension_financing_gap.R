# Create Pension Financing Gap Variable
# Pension Financing Gap = Pension Expenditure - Social Security Contributions
# Both variables as % of GDP

library(dplyr)
library(readr)

print("Creating Pension Financing Gap variable...")

# Read pension expenditure data
pension_data <- read_csv("R/Pension_as_percent_of_GDP_clean.csv")
print(paste("Pension data:", nrow(pension_data), "observations"))

# Read social security contributions data  
social_security_data <- read_csv("R/Social_Security_Contributions_clean.csv")
print(paste("Social security data:", nrow(social_security_data), "observations"))

# Check data coverage
print("\nPension data coverage by country:")
pension_summary <- pension_data %>%
  group_by(Country) %>%
  summarise(
    observations = n(),
    min_year = min(Year),
    max_year = max(Year),
    .groups = 'drop'
  )
print(pension_summary)

print("\nSocial security data coverage by country:")
ss_summary <- social_security_data %>%
  group_by(Country) %>%
  summarise(
    observations = n(),
    min_year = min(Year),
    max_year = max(Year),
    .groups = 'drop'
  )
print(ss_summary)

# Merge the datasets
financing_gap_data <- pension_data %>%
  inner_join(social_security_data, by = c("Country", "Year")) %>%
  mutate(
    Pension_financing_gap = Pension_GDP - Social_security_GDP
  ) %>%
  select(Country, Year, Pension_financing_gap) %>%
  arrange(Country, Year)

print("\nFinancing gap data summary:")
print("Countries found:")
print(unique(financing_gap_data$Country))

if(nrow(financing_gap_data) > 0) {
  print("Year range:")
  print(paste("From", min(financing_gap_data$Year), "to", max(financing_gap_data$Year)))
  
  print("Data availability by country:")
  gap_summary <- financing_gap_data %>%
    group_by(Country) %>%
    summarise(
      observations = n(),
      min_year = min(Year),
      max_year = max(Year),
      min_gap = round(min(Pension_financing_gap, na.rm = TRUE), 2),
      max_gap = round(max(Pension_financing_gap, na.rm = TRUE), 2),
      avg_gap = round(mean(Pension_financing_gap, na.rm = TRUE), 2),
      .groups = 'drop'
    )
  print(gap_summary)
  
  # Check for any extreme values
  print("\nExtreme values check:")
  extreme_values <- financing_gap_data %>%
    filter(abs(Pension_financing_gap) > 10) %>%
    arrange(desc(abs(Pension_financing_gap)))
  
  if(nrow(extreme_values) > 0) {
    print("Values with absolute gap > 10% of GDP:")
    print(extreme_values)
  } else {
    print("No extreme values found (all gaps within ±10% of GDP)")
  }
} else {
  print("No data found after merging")
}

# Save the financing gap data
write_csv(financing_gap_data, "R/Pension_financing_gap_clean.csv")

print("\n✓ Pension financing gap data created and saved to R/Pension_financing_gap_clean.csv")
print(paste("Final dataset:", nrow(financing_gap_data), "observations across", length(unique(financing_gap_data$Country)), "countries"))

print("\nNote: Positive values indicate pension expenditure exceeds contributions (deficit)")
print("Negative values indicate contributions exceed expenditure (surplus)")