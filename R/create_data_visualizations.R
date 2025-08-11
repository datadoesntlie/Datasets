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
library(scales)
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
p1 <- ggplot(master_data %>% filter(!is.na(Female_Labor_Force_Participation)), 
             aes(x = Year, y = Female_Labor_Force_Participation, color = Country, linetype = Country_Group)) +
  geom_line(size = 1.2) +
  scale_color_brewer(type = "qual", palette = "Set3") +
  scale_linetype_manual(values = c("Developed" = "solid", "Developing" = "dashed")) +
  labs(title = "Female Labor Force Participation Rate Trends (1990-2024)",
       subtitle = "Panel of 11 Countries",
       x = "Year", 
       y = "Female Labor Force Participation (%)",
       color = "Country",
       linetype = "Country Group") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 11),
        legend.title = element_text(size = 10)) +
  guides(color = guide_legend(ncol = 1))

ggsave("R/plots/female_labor_participation_trends.png", p1, width = 12, height = 8, dpi = 300)

# B. Total Fertility Rate Trends
p2 <- ggplot(master_data %>% filter(!is.na(Total_Fertility_Rate)), 
             aes(x = Year, y = Total_Fertility_Rate, color = Country, linetype = Country_Group)) +
  geom_line(size = 1.2) +
  scale_color_brewer(type = "qual", palette = "Set3") +
  scale_linetype_manual(values = c("Developed" = "solid", "Developing" = "dashed")) +
  geom_hline(yintercept = 2.1, color = "red", linetype = "dotted", size = 1, alpha = 0.7) +
  annotate("text", x = 2020, y = 2.2, label = "Replacement Rate (2.1)", color = "red", size = 3.5) +
  labs(title = "Total Fertility Rate Trends (1990-2024)",
       subtitle = "Panel of 11 Countries with Replacement Rate Reference",
       x = "Year", 
       y = "Total Fertility Rate (births per woman)",
       color = "Country",
       linetype = "Country Group") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 11),
        legend.title = element_text(size = 10)) +
  guides(color = guide_legend(ncol = 1))

ggsave("R/plots/total_fertility_rate_trends.png", p2, width = 12, height = 8, dpi = 300)

# C. Old Age Dependency Ratio Trends
p3 <- ggplot(master_data %>% filter(!is.na(Old_Age_Dependency_Ratio)), 
             aes(x = Year, y = Old_Age_Dependency_Ratio, color = Country, linetype = Country_Group)) +
  geom_line(size = 1.2) +
  scale_color_brewer(type = "qual", palette = "Set3") +
  scale_linetype_manual(values = c("Developed" = "solid", "Developing" = "dashed")) +
  labs(title = "Old Age Dependency Ratio Trends (1990-2024)",
       subtitle = "Panel of 11 Countries",
       x = "Year", 
       y = "Old Age Dependency Ratio (%)",
       color = "Country",
       linetype = "Country Group") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 11),
        legend.title = element_text(size = 10)) +
  guides(color = guide_legend(ncol = 1))

ggsave("R/plots/old_age_dependency_trends.png", p3, width = 12, height = 8, dpi = 300)

# D. Pension Expenditure Trends
p4 <- ggplot(master_data %>% filter(!is.na(Pension_Expenditure)), 
             aes(x = Year, y = Pension_Expenditure, color = Country, linetype = Country_Group)) +
  geom_line(size = 1.2) +
  scale_color_brewer(type = "qual", palette = "Set3") +
  scale_linetype_manual(values = c("Developed" = "solid", "Developing" = "dashed")) +
  labs(title = "Pension Expenditure as % of GDP Trends (1990-2024)",
       subtitle = "Panel of 11 Countries",
       x = "Year", 
       y = "Pension Expenditure (% of GDP)",
       color = "Country",
       linetype = "Country Group") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 11),
        legend.title = element_text(size = 10)) +
  guides(color = guide_legend(ncol = 1))

ggsave("R/plots/pension_expenditure_trends.png", p4, width = 12, height = 8, dpi = 300)

# E. Pension Financing Gap Trends
p5 <- ggplot(master_data %>% filter(!is.na(Pension_Financing_Gap)), 
             aes(x = Year, y = Pension_Financing_Gap, color = Country, linetype = Country_Group)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, color = "black", linetype = "dotted", alpha = 0.5) +
  scale_color_brewer(type = "qual", palette = "Set3") +
  scale_linetype_manual(values = c("Developed" = "solid", "Developing" = "dashed")) +
  labs(title = "Pension Financing Gap Trends (1990-2024)",
       subtitle = "Panel of 11 Countries (Expenditure minus Contributions)",
       x = "Year", 
       y = "Pension Financing Gap (% of GDP)",
       color = "Country",
       linetype = "Country Group") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 11),
        legend.title = element_text(size = 10)) +
  guides(color = guide_legend(ncol = 1))

ggsave("R/plots/pension_financing_gap_trends.png", p5, width = 12, height = 8, dpi = 300)

# 2. COMBINED MULTI-PANEL TIME SERIES PLOT
combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2,
                             top = "Key Economic and Demographic Trends (1990-2024)")
ggsave("R/plots/combined_trends.png", combined_plot, width = 16, height = 12, dpi = 300)

print("Time series plots created successfully!")
print("Files saved in R/plots/ directory:")
print("- female_labor_participation_trends.png")
print("- total_fertility_rate_trends.png") 
print("- old_age_dependency_trends.png")
print("- pension_expenditure_trends.png")
print("- pension_financing_gap_trends.png")
print("- combined_trends.png")