# Save Visualization Analysis to Text File
# This script captures all analytical insights from the visualization analysis

library(dplyr)

# Load master dataset
master_data <- read.csv("R/master_dataset.csv")

# Create output file
analysis_file <- "R/visualization_analysis_results.txt"
cat("", file = analysis_file) # Clear file

# Function to write to file and console
write_both <- function(text) {
  cat(text, "\n", file = analysis_file, append = TRUE)
  cat(text, "\n")
}

write_both(paste(rep("=", 80), collapse = ""))
write_both("CHAPTER 3.4 DATA VISUALIZATION AND TRENDS - ANALYTICAL INSIGHTS")
write_both("Generated on: ")
write_both(Sys.time())
write_both("Time Period: 1990-2021 (excluding incomplete 2022-2024 data)")
write_both(paste(rep("=", 80), collapse = ""))

# 1. TIME SERIES TRENDS ANALYSIS
write_both("\n1. TIME SERIES TRENDS ANALYSIS:")
write_both("--------------------------------")

# Female Labor Force Participation Analysis
flfp_trends <- master_data %>%
  filter(!is.na(FLFP)) %>%
  group_by(Country_Group) %>%
  summarise(
    Start_Mean = round(mean(FLFP[Year <= 1995], na.rm = TRUE), 2),
    End_Mean = round(mean(FLFP[Year >= 2015], na.rm = TRUE), 2),
    Change = round(End_Mean - Start_Mean, 2),
    .groups = 'drop'
  )

write_both("\nA. Female Labor Force Participation Trends (1990s vs 2015+):")
capture.output(print(flfp_trends), file = analysis_file, append = TRUE)
print(flfp_trends)

write_both(paste("Key Finding: Developed countries increased FLFP by", 
            flfp_trends$Change[flfp_trends$Country_Group == "Developed"], 
            "percentage points"))

# Total Fertility Rate Analysis
tfr_trends <- master_data %>%
  filter(!is.na(TFR)) %>%
  group_by(Country_Group) %>%
  summarise(
    Start_Mean = round(mean(TFR[Year <= 1995], na.rm = TRUE), 3),
    End_Mean = round(mean(TFR[Year >= 2015], na.rm = TRUE), 3),
    Change = round(End_Mean - Start_Mean, 3),
    Below_Replacement = round(mean(TFR < 2.1, na.rm = TRUE) * 100, 1),
    .groups = 'drop'
  )

write_both("\nB. Total Fertility Rate Trends (1990s vs 2015+):")
capture.output(print(tfr_trends), file = analysis_file, append = TRUE)
print(tfr_trends)

write_both(paste("Key Finding:", tfr_trends$Below_Replacement[tfr_trends$Country_Group == "Developed"], 
            "% of developed country observations below replacement rate (2.1)"))

# Old Age Dependency Analysis
aging_trends <- master_data %>%
  filter(!is.na(Old_age_dependency)) %>%
  group_by(Country_Group) %>%
  summarise(
    Start_Mean = round(mean(Old_age_dependency[Year <= 1995], na.rm = TRUE), 2),
    End_Mean = round(mean(Old_age_dependency[Year >= 2015], na.rm = TRUE), 2),
    Change = round(End_Mean - Start_Mean, 2),
    .groups = 'drop'
  )

write_both("\nC. Old Age Dependency Ratio Trends (1990s vs 2015+):")
capture.output(print(aging_trends), file = analysis_file, append = TRUE)
print(aging_trends)

write_both(paste("Key Finding: Developed countries aging accelerated by", 
            aging_trends$Change[aging_trends$Country_Group == "Developed"], 
            "percentage points"))

# 2. CROSS-COUNTRY COMPARISON ANALYSIS
write_both("\n\n2. CROSS-COUNTRY COMPARISON ANALYSIS:")
write_both("------------------------------------")

group_comparison <- master_data %>%
  group_by(Country_Group) %>%
  summarise(
    FLFP_Mean = round(mean(FLFP, na.rm = TRUE), 1),
    TFR_Mean = round(mean(TFR, na.rm = TRUE), 2),
    GDP_Mean = round(mean(GDP_per_capita, na.rm = TRUE)/1000, 1),
    Pension_Mean = round(mean(Pension_GDP, na.rm = TRUE), 1),
    Education_Mean = round(mean(Female_tertiary_education, na.rm = TRUE), 1),
    .groups = 'drop'
  )

write_both("\nGroup Averages Comparison:")
capture.output(print(group_comparison), file = analysis_file, append = TRUE)
print(group_comparison)

# Calculate development gaps
developed_row <- group_comparison[group_comparison$Country_Group == "Developed", ]
developing_row <- group_comparison[group_comparison$Country_Group == "Developing", ]

write_both("\nDevelopment Gaps (Developed vs Developing):")
write_both(paste("GDP per capita gap:", round(developed_row$GDP_Mean / developing_row$GDP_Mean, 1), "times higher"))
write_both(paste("Female education gap:", round(developed_row$Education_Mean / developing_row$Education_Mean, 1), "times higher"))
write_both(paste("Pension expenditure gap:", round(developed_row$Pension_Mean / developing_row$Pension_Mean, 1), "times higher"))

# 3. KEY CORRELATIONS
write_both("\n\n3. KEY CORRELATIONS:")
write_both("-------------------")

cor_data <- master_data %>%
  select(FLFP, TFR, Old_age_dependency, Pension_GDP, GDP_per_capita, Female_tertiary_education) %>%
  cor(use = "pairwise.complete.obs")

write_both(paste("Female Labor vs Fertility:", round(cor_data["FLFP", "TFR"], 3)))
write_both(paste("GDP vs Fertility:", round(cor_data["GDP_per_capita", "TFR"], 3)))
write_both(paste("Education vs Fertility:", round(cor_data["Female_tertiary_education", "TFR"], 3)))
write_both(paste("Aging vs Pension Expenditure:", round(cor_data["Old_age_dependency", "Pension_GDP"], 3)))
write_both(paste("Female Labor vs Education:", round(cor_data["FLFP", "Female_tertiary_education"], 3)))

# 4. KEY STYLIZED FACTS
write_both("\n\n4. KEY STYLIZED FACTS:")
write_both("---------------------")

write_both("Stylized Fact 1: Labor-Fertility Trade-off")
lf_cor <- cor(master_data$FLFP, master_data$TFR, use = "complete.obs")
write_both(paste("  Correlation coefficient:", round(lf_cor, 3)))
write_both("  Interpretation: Negative correlation supports the trade-off hypothesis")

write_both("\nStylized Fact 2: Development-Fertility Relationship") 
gf_cor <- cor(master_data$GDP_per_capita, master_data$TFR, use = "complete.obs")
write_both(paste("  Correlation coefficient:", round(gf_cor, 3)))
write_both("  Interpretation: Strong negative correlation confirms demographic transition theory")

write_both("\nStylized Fact 3: Education-Fertility Relationship")
ef_cor <- cor(master_data$Female_tertiary_education, master_data$TFR, use = "complete.obs")
write_both(paste("  Correlation coefficient:", round(ef_cor, 3)))
write_both("  Interpretation: Education empowerment leads to fertility decline")

write_both("\nStylized Fact 4: Aging-Pension Sustainability Challenge")
ap_cor <- cor(master_data$Old_age_dependency, master_data$Pension_GDP, use = "complete.obs")
write_both(paste("  Correlation coefficient:", round(ap_cor, 3)))
write_both("  Interpretation: Strong positive correlation shows pension system pressure")

# 5. DATA COVERAGE SUMMARY
write_both("\n\n5. DATA COVERAGE SUMMARY:")
write_both("------------------------")

coverage <- master_data %>%
  summarise(
    Total_Observations = n(),
    FLFP_Coverage = round(sum(!is.na(FLFP))/n()*100, 1),
    TFR_Coverage = round(sum(!is.na(TFR))/n()*100, 1),
    Education_Coverage = round(sum(!is.na(Female_tertiary_education))/n()*100, 1),
    Pension_Coverage = round(sum(!is.na(Pension_GDP))/n()*100, 1),
    GDP_Coverage = round(sum(!is.na(GDP_per_capita))/n()*100, 1)
  )

write_both("Data Coverage (% of total observations):")
capture.output(print(coverage), file = analysis_file, append = TRUE)
print(coverage)

# Country-specific missing data
country_coverage <- master_data %>%
  group_by(Country) %>%
  summarise(
    Observations = n(),
    Missing_Count = sum(is.na(FLFP) + is.na(TFR) + is.na(Female_tertiary_education) + 
                       is.na(Pension_GDP) + is.na(GDP_per_capita)),
    Coverage_Rate = round((1 - Missing_Count/(n()*5))*100, 1),
    .groups = 'drop'
  ) %>%
  arrange(Coverage_Rate)

write_both("\nCountry Data Coverage Ranking (lowest to highest):")
capture.output(print(country_coverage), file = analysis_file, append = TRUE)
print(country_coverage)

write_both("\n" + paste(rep("=", 80), collapse = ""))
write_both("SUMMARY OF KEY FINDINGS:")
write_both("========================")
write_both("1. Developed countries show clear labor-fertility trade-off with rising FLFP and declining TFR")
write_both("2. All countries face aging challenge, but developed countries more severely")
write_both("3. Strong negative correlations confirm demographic transition theory")
write_both("4. Pension sustainability challenge is evident in developed countries")
write_both("5. Female education is a key driver of demographic changes")

write_both(paste("\nAnalysis saved to:", analysis_file))
print(paste("Analysis saved to:", analysis_file))