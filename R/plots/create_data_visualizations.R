# Data Visualization and Trends Script
# Chapter 3.4 of Econometric Thesis
# Creates comprehensive visualizations for panel data analysis

# Load required libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(corrplot)
library(RColorBrewer)
library(gridExtra)
library(viridis)

# Load master dataset
master_data <- read.csv("R/master_dataset.csv")

# Define country groups
developed_countries <- c("Greece", "Japan", "Italy", "Spain", "Germany", "France", "Sweden", "South Korea")
developing_countries <- c("Brazil", "Mexico", "Chile")

# Add country group variable
master_data <- master_data %>%
  mutate(Country_Group = ifelse(Country %in% developed_countries, "Developed", "Developing"))

# Create output directory for plots
if (!dir.exists("R/plots")) {
  dir.create("R/plots", recursive = TRUE)
}

# 1. TIME SERIES PLOTS FOR KEY VARIABLES

# A. Female Labor Force Participation Trends
p1 <- ggplot(master_data %>% filter(!is.na(FLFP)), 
             aes(x = Year, y = FLFP, color = Country, linetype = Country_Group)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Brazil" = "#E31A1C", "Chile" = "#FF7F00", "Mexico" = "#1F78B4",
                                "France" = "#33A02C", "Germany" = "#6A3D9A", "Greece" = "#B15928",
                                "Italy" = "#A6CEE3", "Japan" = "#B2DF8A", "South Korea" = "#FB9A99",
                                "Spain" = "#FDBF6F", "Sweden" = "#CAB2D6")) +
  scale_linetype_manual(values = c("Developed" = "solid", "Developing" = "dashed")) +
  labs(title = "Female Labor Force Participation Rate Trends (1990-2021)",
       subtitle = "Panel of 11 Countries (Solid: Developed, Dashed: Developing)",
       x = "Year", 
       y = "Female Labor Force Participation (%)",
       color = "Country") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 11),
        legend.title = element_text(size = 10)) +
  guides(color = guide_legend(ncol = 1), linetype = "none")

ggsave("R/plots/female_labor_participation_trends.png", p1, width = 12, height = 8, dpi = 300)

# B. Total Fertility Rate Trends
p2 <- ggplot(master_data %>% filter(!is.na(TFR)), 
             aes(x = Year, y = TFR, color = Country, linetype = Country_Group)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Brazil" = "#E31A1C", "Chile" = "#FF7F00", "Mexico" = "#1F78B4",
                                "France" = "#33A02C", "Germany" = "#6A3D9A", "Greece" = "#B15928",
                                "Italy" = "#A6CEE3", "Japan" = "#B2DF8A", "South Korea" = "#FB9A99",
                                "Spain" = "#FDBF6F", "Sweden" = "#CAB2D6")) +
  scale_linetype_manual(values = c("Developed" = "solid", "Developing" = "dashed")) +
  geom_hline(yintercept = 2.1, color = "red", linetype = "dotted", size = 1, alpha = 0.7) +
  annotate("text", x = 2020, y = 2.2, label = "Replacement Rate (2.1)", color = "red", size = 3.5) +
  labs(title = "Total Fertility Rate Trends (1990-2021)",
       subtitle = "Panel of 11 Countries (Solid: Developed, Dashed: Developing)",
       x = "Year", 
       y = "Total Fertility Rate (births per woman)",
       color = "Country") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 11),
        legend.title = element_text(size = 10)) +
  guides(color = guide_legend(ncol = 1), linetype = "none")

ggsave("R/plots/total_fertility_rate_trends.png", p2, width = 12, height = 8, dpi = 300)

# C. Old Age Dependency Ratio Trends
p3 <- ggplot(master_data %>% filter(!is.na(Old_age_dependency)), 
             aes(x = Year, y = Old_age_dependency, color = Country, linetype = Country_Group)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Brazil" = "#E31A1C", "Chile" = "#FF7F00", "Mexico" = "#1F78B4",
                                "France" = "#33A02C", "Germany" = "#6A3D9A", "Greece" = "#B15928",
                                "Italy" = "#A6CEE3", "Japan" = "#B2DF8A", "South Korea" = "#FB9A99",
                                "Spain" = "#FDBF6F", "Sweden" = "#CAB2D6")) +
  scale_linetype_manual(values = c("Developed" = "solid", "Developing" = "dashed")) +
  labs(title = "Old Age Dependency Ratio Trends (1990-2021)",
       subtitle = "Panel of 11 Countries (Solid: Developed, Dashed: Developing)",
       x = "Year", 
       y = "Old Age Dependency Ratio (%)",
       color = "Country") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 11),
        legend.title = element_text(size = 10)) +
  guides(color = guide_legend(ncol = 1), linetype = "none")

ggsave("R/plots/old_age_dependency_trends.png", p3, width = 12, height = 8, dpi = 300)

# D. Pension Expenditure Trends
p4 <- ggplot(master_data %>% filter(!is.na(Pension_GDP)), 
             aes(x = Year, y = Pension_GDP, color = Country, linetype = Country_Group)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Brazil" = "#E31A1C", "Chile" = "#FF7F00", "Mexico" = "#1F78B4",
                                "France" = "#33A02C", "Germany" = "#6A3D9A", "Greece" = "#B15928",
                                "Italy" = "#A6CEE3", "Japan" = "#B2DF8A", "South Korea" = "#FB9A99",
                                "Spain" = "#FDBF6F", "Sweden" = "#CAB2D6")) +
  scale_linetype_manual(values = c("Developed" = "solid", "Developing" = "dashed")) +
  labs(title = "Pension Expenditure as % of GDP Trends (1990-2021)",
       subtitle = "Panel of 11 Countries (Solid: Developed, Dashed: Developing)",
       x = "Year", 
       y = "Pension Expenditure (% of GDP)",
       color = "Country") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 11),
        legend.title = element_text(size = 10)) +
  guides(color = guide_legend(ncol = 1), linetype = "none")

ggsave("R/plots/pension_expenditure_trends.png", p4, width = 12, height = 8, dpi = 300)

# E. Pension Financing Gap Trends
p5 <- ggplot(master_data %>% filter(!is.na(Pension_financing_gap)), 
             aes(x = Year, y = Pension_financing_gap, color = Country, linetype = Country_Group)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, color = "black", linetype = "dotted", alpha = 0.5) +
  scale_color_manual(values = c("Brazil" = "#E31A1C", "Chile" = "#FF7F00", "Mexico" = "#1F78B4",
                                "France" = "#33A02C", "Germany" = "#6A3D9A", "Greece" = "#B15928",
                                "Italy" = "#A6CEE3", "Japan" = "#B2DF8A", "South Korea" = "#FB9A99",
                                "Spain" = "#FDBF6F", "Sweden" = "#CAB2D6")) +
  scale_linetype_manual(values = c("Developed" = "solid", "Developing" = "dashed")) +
  labs(title = "Pension Financing Gap Trends (1990-2021)",
       subtitle = "Panel of 11 Countries (Solid: Developed, Dashed: Developing)",
       x = "Year", 
       y = "Pension Financing Gap (% of GDP)",
       color = "Country") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 11),
        legend.title = element_text(size = 10)) +
  guides(color = guide_legend(ncol = 1), linetype = "none")

ggsave("R/plots/pension_financing_gap_trends.png", p5, width = 12, height = 8, dpi = 300)

# 2. COMBINED MULTI-PANEL TIME SERIES PLOT
combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2,
                             top = "Key Economic and Demographic Trends (1990-2021)")
ggsave("R/plots/combined_trends.png", combined_plot, width = 16, height = 12, dpi = 300)

# 3. CROSS-COUNTRY COMPARISON CHARTS

# A. Box plots comparing developed vs developing countries
comparison_data <- master_data %>%
  select(Country, Country_Group, FLFP, TFR, 
         Old_age_dependency, Pension_GDP, GDP_per_capita, Female_tertiary_education) %>%
  pivot_longer(cols = c(FLFP, TFR, 
                       Old_age_dependency, Pension_GDP, GDP_per_capita, Female_tertiary_education),
               names_to = "Variable", values_to = "Value") %>%
  filter(!is.na(Value)) %>%
  mutate(
    Variable = case_when(
      Variable == "FLFP" ~ "Female Labor\nParticipation (%)",
      Variable == "TFR" ~ "Total Fertility\nRate",
      Variable == "Old_age_dependency" ~ "Old Age\nDependency (%)",
      Variable == "Pension_GDP" ~ "Pension\nExpenditure (% GDP)",
      Variable == "GDP_per_capita" ~ "GDP per Capita\n(2017 PPP $)",
      Variable == "Female_tertiary_education" ~ "Female Tertiary\nEducation (%)",
      TRUE ~ Variable
    )
  )

p6 <- ggplot(comparison_data, aes(x = Country_Group, y = Value, fill = Country_Group)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 0.8) +
  facet_wrap(~Variable, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("Developed" = "#1B9E77", "Developing" = "#D95F02")) +
  labs(title = "Cross-Country Comparison: Developed vs Developing Countries",
       subtitle = "Distribution of Key Variables (1990-2021)",
       x = "Country Group",
       y = "Variable Value",
       fill = "Country Group") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom"
  )

ggsave("R/plots/cross_country_comparison.png", p6, width = 14, height = 10, dpi = 300)

# B. Average levels comparison bar chart
avg_comparison <- master_data %>%
  group_by(Country_Group) %>%
  summarise(
    `Female Labor\nParticipation (%)` = round(mean(FLFP, na.rm = TRUE), 1),
    `Total Fertility\nRate` = round(mean(TFR, na.rm = TRUE), 2),
    `Old Age\nDependency (%)` = round(mean(Old_age_dependency, na.rm = TRUE), 1),
    `Pension\nExpenditure (% GDP)` = round(mean(Pension_GDP, na.rm = TRUE), 1),
    `GDP per Capita\n(thousands 2017 PPP $)` = round(mean(GDP_per_capita, na.rm = TRUE)/1000, 1),
    `Female Tertiary\nEducation (%)` = round(mean(Female_tertiary_education, na.rm = TRUE), 1),
    .groups = 'drop'
  ) %>%
  pivot_longer(cols = -Country_Group, names_to = "Variable", values_to = "Average")

p7 <- ggplot(avg_comparison, aes(x = Variable, y = Average, fill = Country_Group)) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("Developed" = "#1B9E77", "Developing" = "#D95F02")) +
  labs(title = "Average Levels: Developed vs Developing Countries",
       subtitle = "Mean Values Across All Years (1990-2021)",
       x = "Variable",
       y = "Average Value",
       fill = "Country Group") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "bottom"
  ) +
  geom_text(aes(label = Average), position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 3.5, fontface = "bold")

ggsave("R/plots/average_levels_comparison.png", p7, width = 12, height = 8, dpi = 300)

# 4. CORRELATION MATRIX HEATMAP

# Calculate correlation matrix for numeric variables
numeric_data <- master_data %>%
  select(FLFP, TFR, Old_age_dependency,
         Pension_GDP, Social_security_GDP, GDP_per_capita, Life_expectancy_65,
         Female_tertiary_education, Urban_rate, CPR, Pension_financing_gap) %>%
  rename(
    `Female Labor\nParticipation` = FLFP,
    `Total Fertility\nRate` = TFR,
    `Old Age\nDependency` = Old_age_dependency,
    `Pension\nExpenditure` = Pension_GDP,
    `Social Security\nContributions` = Social_security_GDP,
    `GDP per\nCapita` = GDP_per_capita,
    `Life Expectancy\nat 65` = Life_expectancy_65,
    `Female Tertiary\nEducation` = Female_tertiary_education,
    `Urban\nPopulation` = Urban_rate,
    `Contraceptive\nPrevalence` = CPR,
    `Pension\nFinancing Gap` = Pension_financing_gap
  )

# Calculate correlation matrix
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

# Create correlation heatmap
png("R/plots/correlation_heatmap.png", width = 12, height = 10, units = "in", res = 300)
corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         order = "hclust",
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.8,
         col = colorRampPalette(c("#d73027", "#f46d43", "#fdae61", "#fee08b", 
                                  "#e6f598", "#abdda4", "#66c2a5", "#3288bd"))(200),
         addCoef.col = "black",
         number.cex = 0.7,
         title = "Correlation Matrix: Key Economic and Demographic Variables",
         mar = c(0,0,2,0))
dev.off()

# 5. KEY STYLIZED FACTS VISUALIZATION

# A. Female Labor vs Fertility Trade-off
p8 <- ggplot(master_data %>% filter(!is.na(FLFP), !is.na(TFR)), 
             aes(x = FLFP, y = TFR)) +
  geom_point(aes(color = Country_Group, size = GDP_per_capita), alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("Developed" = "#1B9E77", "Developing" = "#D95F02")) +
  scale_size_continuous(name = "GDP per Capita", guide = "legend") +
  labs(title = "The Trade-off: Female Labor Participation vs Fertility",
       subtitle = "Panel Data 1990-2021 (11 Countries)",
       x = "Female Labor Force Participation (%)",
       y = "Total Fertility Rate",
       color = "Country Group",
       size = "GDP per Capita") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11),
    legend.title = element_text(size = 10)
  )

ggsave("R/plots/labor_fertility_tradeoff.png", p8, width = 12, height = 8, dpi = 300)

# B. Pension Sustainability Challenge
p9 <- ggplot(master_data %>% filter(!is.na(Old_age_dependency), !is.na(Pension_GDP)), 
             aes(x = Old_age_dependency, y = Pension_GDP)) +
  geom_point(aes(color = Country_Group, size = TFR), alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("Developed" = "#1B9E77", "Developing" = "#D95F02")) +
  labs(title = "Pension Sustainability Challenge: Aging and Expenditure",
       subtitle = "Panel Data 1990-2021 (11 Countries)",
       x = "Old Age Dependency Ratio (%)",
       y = "Pension Expenditure (% of GDP)",
       color = "Country Group",
       size = "Total Fertility Rate") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11),
    legend.title = element_text(size = 10)
  )

ggsave("R/plots/pension_sustainability_challenge.png", p9, width = 12, height = 8, dpi = 300)

# C. Development and Demographic Transition
p10 <- ggplot(master_data %>% filter(!is.na(GDP_per_capita), !is.na(TFR)), 
              aes(x = GDP_per_capita/1000, y = TFR)) +
  geom_point(aes(color = Country, shape = Country_Group), alpha = 0.7, size = 2) +
  geom_smooth(method = "loess", se = TRUE, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("Brazil" = "#E31A1C", "Chile" = "#FF7F00", "Mexico" = "#1F78B4",
                                "France" = "#33A02C", "Germany" = "#6A3D9A", "Greece" = "#B15928",
                                "Italy" = "#A6CEE3", "Japan" = "#B2DF8A", "South Korea" = "#FB9A99",
                                "Spain" = "#FDBF6F", "Sweden" = "#CAB2D6")) +
  scale_shape_manual(values = c("Developed" = 16, "Developing" = 17)) +
  geom_hline(yintercept = 2.1, color = "red", linetype = "dotted", alpha = 0.7) +
  annotate("text", x = 40, y = 2.2, label = "Replacement Rate", color = "red", size = 3.5) +
  labs(title = "Economic Development and Demographic Transition",
       subtitle = "Panel Data 1990-2021 (11 Countries)",
       x = "GDP per Capita (thousands 2017 PPP $)",
       y = "Total Fertility Rate",
       color = "Country",
       shape = "Country Group") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11),
    legend.title = element_text(size = 10)
  ) +
  guides(color = guide_legend(ncol = 2))

ggsave("R/plots/development_demographic_transition.png", p10, width = 12, height = 8, dpi = 300)

# D. Educational Empowerment and Fertility
p11 <- ggplot(master_data %>% filter(!is.na(Female_tertiary_education), !is.na(TFR)), 
              aes(x = Female_tertiary_education, y = TFR)) +
  geom_point(aes(color = Country_Group, size = FLFP), alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("Developed" = "#1B9E77", "Developing" = "#D95F02")) +
  labs(title = "Educational Empowerment and Fertility Decline",
       subtitle = "Panel Data 1990-2021 (11 Countries)",
       x = "Female Tertiary Education Rate (%)",
       y = "Total Fertility Rate",
       color = "Country Group",
       size = "Female Labor\nParticipation (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 11),
    legend.title = element_text(size = 10)
  )

ggsave("R/plots/education_fertility_relationship.png", p11, width = 12, height = 8, dpi = 300)

print("All visualizations created successfully!")
print("\nFiles saved in R/plots/ directory:")
print("=== TIME SERIES PLOTS ===")
print("- female_labor_participation_trends.png")
print("- total_fertility_rate_trends.png") 
print("- old_age_dependency_trends.png")
print("- pension_expenditure_trends.png")
print("- pension_financing_gap_trends.png")
print("- combined_trends.png")
print("\n=== CROSS-COUNTRY COMPARISONS ===")
print("- cross_country_comparison.png")
print("- average_levels_comparison.png")
print("\n=== CORRELATION ANALYSIS ===")
print("- correlation_heatmap.png")
print("\n=== KEY STYLIZED FACTS ===")
print("- labor_fertility_tradeoff.png")
print("- pension_sustainability_challenge.png") 
print("- development_demographic_transition.png")
print("- education_fertility_relationship.png")
print("\n✓ Chapter 3.4 Data Visualization and Trends completed!")

# 6. ANALYTICAL INSIGHTS FOR EACH VISUALIZATION
print(paste0("\n", paste(rep("=", 80), collapse = "")))
print("CHAPTER 3.4 DATA VISUALIZATION AND TRENDS - ANALYTICAL INSIGHTS")
print(paste(rep("=", 80), collapse = ""))

# A. Time Series Analysis
print("\n1. TIME SERIES TRENDS ANALYSIS:")
print("--------------------------------")

# Female Labor Force Participation Analysis
flfp_trends <- master_data %>%
  filter(!is.na(FLFP)) %>%
  group_by(Country_Group) %>%
  summarise(
    Start_Mean = mean(FLFP[Year <= 1995], na.rm = TRUE),
    End_Mean = mean(FLFP[Year >= 2015], na.rm = TRUE),
    Change = End_Mean - Start_Mean,
    .groups = 'drop'
  )

print("Female Labor Force Participation (1990s vs 2015+):")
print(flfp_trends)

# Total Fertility Rate Analysis
tfr_trends <- master_data %>%
  filter(!is.na(TFR)) %>%
  group_by(Country_Group) %>%
  summarise(
    Start_Mean = mean(TFR[Year <= 1995], na.rm = TRUE),
    End_Mean = mean(TFR[Year >= 2015], na.rm = TRUE),
    Change = End_Mean - Start_Mean,
    Below_Replacement = mean(TFR < 2.1, na.rm = TRUE) * 100,
    .groups = 'drop'
  )

print("\nTotal Fertility Rate (1990s vs 2015+):")
print(tfr_trends)

# B. Cross-Country Comparison Analysis
print("\n\n2. CROSS-COUNTRY COMPARISON ANALYSIS:")
print("------------------------------------")

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

print("Group Averages Comparison:")
print(group_comparison)

# C. Key Correlations
print("\n\n3. KEY CORRELATIONS:")
print("-------------------")

cor_data <- master_data %>%
  select(FLFP, TFR, Old_age_dependency, Pension_GDP, GDP_per_capita, Female_tertiary_education) %>%
  cor(use = "pairwise.complete.obs")

print(paste("Female Labor vs Fertility:", round(cor_data["FLFP", "TFR"], 3)))
print(paste("GDP vs Fertility:", round(cor_data["GDP_per_capita", "TFR"], 3)))
print(paste("Education vs Fertility:", round(cor_data["Female_tertiary_education", "TFR"], 3)))
print(paste("Aging vs Pension Expenditure:", round(cor_data["Old_age_dependency", "Pension_GDP"], 3)))