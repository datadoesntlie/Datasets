library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(gridExtra)
library(viridis)
library(scales)

# Load the robustness comparison results
load("../robustness_comparison_results.RData")

# Read the detailed coefficient comparison
detailed_comp <- read_csv("../6.detailed_coefficient_comparison.csv")

cat("=== CREATING ROBUSTNESS VISUALIZATION PLOTS ===\n\n")

# =============================================================================
# 1. ROBUSTNESS COMPARISON PLOT: Coefficient Changes Across Specifications
# =============================================================================

cat("Creating coefficient comparison plots...\n")

# Prepare data for coefficient plots
coef_data <- detailed_comp %>%
  # Create standardized variable names
  mutate(
    Variable_Standard = case_when(
      Variable %in% c("d_FLFP", "FLFP_lag2") ~ "FLFP",
      Variable %in% c("CPR", "CPR_lag3") ~ "CPR",
      Variable %in% c("TFR", "TFR_lag5") ~ "TFR",
      TRUE ~ Variable
    ),
    # Extract dependent variable from model name
    Dep_Variable = case_when(
      grepl("d_Pension_GDP", Model) ~ "Pension GDP (%)",
      grepl("d_Old_age_dependency", Model) ~ "Old-age Dependency",
      grepl("d_Pension_financing_gap", Model) ~ "Pension Financing Gap",
      grepl("d_Social_security_GDP", Model) ~ "Social Security GDP (%)",
      TRUE ~ "Unknown"
    ),
    # Extract model type
    Model_Spec = case_when(
      grepl("Main", Model) ~ "Main Model",
      grepl("Lagged", Model) ~ "Lagged Variables",
      grepl("No_Outliers", Model) ~ "No Outliers",
      TRUE ~ "Other"
    ),
    # Calculate confidence intervals
    CI_Lower = Coefficient - 1.96 * Std_Error,
    CI_Upper = Coefficient + 1.96 * Std_Error,
    # Significance indicator
    Significant = P_Value < 0.05
  ) %>%
  filter(Variable_Standard %in% c("FLFP", "CPR", "TFR"))

# Create coefficient comparison plots for each dependent variable
create_coef_plot <- function(dep_var) {
  plot_data <- coef_data %>% filter(Dep_Variable == dep_var)
  
  if(nrow(plot_data) == 0) return(NULL)
  
  ggplot(plot_data, aes(x = Model_Spec, y = Coefficient, color = Variable_Standard)) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_point(size = 3, position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                  width = 0.2, position = position_dodge(width = 0.3)) +
    geom_point(data = plot_data %>% filter(Significant), 
               shape = 8, size = 2, color = "red", 
               position = position_dodge(width = 0.3)) +
    scale_color_manual(values = c("FLFP" = "#1f77b4", "CPR" = "#ff7f0e", "TFR" = "#2ca02c"),
                       name = "Variable") +
    labs(
      title = paste("Coefficient Robustness:", dep_var),
      subtitle = "Error bars: 95% CI, Red stars: p<0.05",
      x = "Model Specification",
      y = "Coefficient Estimate"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
}

# Create plots for all dependent variables
dep_vars <- unique(coef_data$Dep_Variable)
coef_plots <- list()

for(dep_var in dep_vars) {
  plot_obj <- create_coef_plot(dep_var)
  if(!is.null(plot_obj)) {
    coef_plots[[dep_var]] <- plot_obj
  }
}

# Save individual coefficient plots
for(i in seq_along(coef_plots)) {
  dep_var <- names(coef_plots)[i]
  filename <- paste0("coefficient_robustness_", gsub("[^A-Za-z0-9]", "_", dep_var), ".png")
  ggsave(filename, coef_plots[[i]], width = 10, height = 6, dpi = 300)
  cat("Saved:", filename, "\n")
}

# Create combined coefficient plot
if(length(coef_plots) > 1) {
  combined_coef <- do.call(grid.arrange, c(coef_plots, ncol = 2))
  ggsave("combined_coefficient_robustness.png", combined_coef, width = 16, height = 12, dpi = 300)
  cat("Saved: combined_coefficient_robustness.png\n")
}

# =============================================================================
# 2. MODEL PERFORMANCE COMPARISON: R² Values Across Models
# =============================================================================

cat("\nCreating R-squared comparison plot...\n")

# Extract R² values from the detailed comparison results
r2_data <- data.frame(
  Model = character(),
  Dependent_Variable = character(),
  R_Squared = numeric(),
  Model_Type = character(),
  stringsAsFactors = FALSE
)

# Function to safely extract R² from model objects
safe_r2 <- function(model_obj) {
  tryCatch({
    r2_vals <- summary(model_obj)$r.squared
    if(length(r2_vals) > 0) return(r2_vals[1])
    return(NA)
  }, error = function(e) NA)
}

# Load robustness results to extract R² values
load("../robustness_results.RData")

# Extract R² for each model type
dep_vars_code <- c("d_Pension_GDP", "d_Old_age_dependency", 
                   "d_Pension_financing_gap", "d_Social_security_GDP")

dep_vars_labels <- c("Pension GDP (%)", "Old-age Dependency", 
                     "Pension Financing Gap", "Social Security GDP (%)")

for(i in seq_along(dep_vars_code)) {
  dep_var <- dep_vars_code[i]
  dep_label <- dep_vars_labels[i]
  
  # Lagged models
  if(dep_var %in% names(robustness_results$lagged_models)) {
    r2_val <- safe_r2(robustness_results$lagged_models[[dep_var]]$model)
    r2_data <- rbind(r2_data, data.frame(
      Model = "Lagged Variables",
      Dependent_Variable = dep_label,
      R_Squared = r2_val,
      Model_Type = robustness_results$lagged_models[[dep_var]]$model_type
    ))
  }
  
  # Outlier exclusion models
  if(dep_var %in% names(robustness_results$outlier_exclusion)) {
    r2_val <- safe_r2(robustness_results$outlier_exclusion[[dep_var]]$model)
    r2_data <- rbind(r2_data, data.frame(
      Model = "No Outliers",
      Dependent_Variable = dep_label,
      R_Squared = r2_val,
      Model_Type = robustness_results$outlier_exclusion[[dep_var]]$model_type
    ))
  }
}

# Add main model R² (run quick models to get R²)
library(plm)
data <- read_csv("../master_dataset.csv")

data_clean <- data %>%
  filter(!is.na(Country) & !is.na(Year)) %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%
  mutate(
    d_Pension_GDP = Pension_GDP - lag(Pension_GDP),
    d_Old_age_dependency = Old_age_dependency - lag(Old_age_dependency),
    d_Pension_financing_gap = Pension_financing_gap - lag(Pension_financing_gap),
    d_Social_security_GDP = Social_security_GDP - lag(Social_security_GDP),
    d_FLFP = FLFP - lag(FLFP),
    d_GDP_per_capita = GDP_per_capita - lag(GDP_per_capita),
    d_Life_expectancy_65 = Life_expectancy_65 - lag(Life_expectancy_65),
    d_Female_tertiary_education = Female_tertiary_education - lag(Female_tertiary_education),
    d_Urban_rate = Urban_rate - lag(Urban_rate)
  ) %>%
  ungroup()

pdata <- pdata.frame(data_clean, index = c("Country", "Year"))

# Get main model R² values
for(i in seq_along(dep_vars_code)) {
  dep_var <- dep_vars_code[i]
  dep_label <- dep_vars_labels[i]
  
  model_data <- pdata[!is.na(pdata[[dep_var]]) & 
                     !is.na(pdata$d_FLFP) & 
                     !is.na(pdata$CPR) & 
                     !is.na(pdata$TFR), ]
  
  if(nrow(model_data) >= 30) {
    formula_str <- paste(dep_var, "~ d_FLFP + CPR + TFR + d_GDP_per_capita + d_Life_expectancy_65 + d_Female_tertiary_education + d_Urban_rate")
    model_formula <- as.formula(formula_str)
    
    tryCatch({
      main_fe <- plm(model_formula, data = model_data, model = "within")
      main_re <- plm(model_formula, data = model_data, model = "random")
      
      hausman_main <- phtest(main_fe, main_re)
      main_model <- if(hausman_main$p.value < 0.05) main_fe else main_re
      main_model_type <- if(hausman_main$p.value < 0.05) "Fixed Effects" else "Random Effects"
      
      r2_val <- safe_r2(main_model)
      r2_data <- rbind(r2_data, data.frame(
        Model = "Main Model",
        Dependent_Variable = dep_label,
        R_Squared = r2_val,
        Model_Type = main_model_type
      ))
    }, error = function(e) {
      cat("Error with main model for", dep_var, ":", e$message, "\n")
    })
  }
}

# Create R² comparison plot
r2_plot <- r2_data %>%
  filter(!is.na(R_Squared)) %>%
  ggplot(aes(x = Model, y = R_Squared, fill = Dependent_Variable)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.3f", R_Squared)), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_viridis_d(name = "Dependent Variable") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1), limits = c(0, max(r2_data$R_Squared, na.rm = TRUE) * 1.1)) +
  labs(
    title = "Model Performance Comparison Across Robustness Checks",
    subtitle = "R² values for different model specifications",
    x = "Model Specification",
    y = "R-squared"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

ggsave("model_performance_comparison.png", r2_plot, width = 12, height = 8, dpi = 300)
cat("Saved: model_performance_comparison.png\n")

# =============================================================================
# 3. SIGNIFICANCE PATTERN HEATMAP: Variable Significance Across Models
# =============================================================================

cat("\nCreating significance pattern heatmap...\n")

# Prepare significance data
sig_data <- detailed_comp %>%
  mutate(
    Variable_Standard = case_when(
      Variable %in% c("d_FLFP", "FLFP_lag2") ~ "FLFP",
      Variable %in% c("CPR", "CPR_lag3") ~ "CPR",
      Variable %in% c("TFR", "TFR_lag5") ~ "TFR",
      TRUE ~ Variable
    ),
    Dep_Variable = case_when(
      grepl("d_Pension_GDP", Model) ~ "Pension GDP",
      grepl("d_Old_age_dependency", Model) ~ "Old-age Dependency",
      grepl("d_Pension_financing_gap", Model) ~ "Pension Gap",
      grepl("d_Social_security_GDP", Model) ~ "Social Security",
      TRUE ~ "Unknown"
    ),
    Model_Spec = case_when(
      grepl("Main", Model) ~ "Main",
      grepl("Lagged", Model) ~ "Lagged",
      grepl("No_Outliers", Model) ~ "No Outliers",
      TRUE ~ "Other"
    ),
    # Significance levels
    Sig_Level = case_when(
      P_Value < 0.001 ~ "***",
      P_Value < 0.01 ~ "**", 
      P_Value < 0.05 ~ "*",
      P_Value < 0.10 ~ "†",
      TRUE ~ ""
    ),
    # Numeric significance for color mapping
    Sig_Numeric = case_when(
      P_Value < 0.001 ~ 4,
      P_Value < 0.01 ~ 3,
      P_Value < 0.05 ~ 2,
      P_Value < 0.10 ~ 1,
      TRUE ~ 0
    ),
    # Combine model and dependent variable
    Model_Full = paste(Model_Spec, Dep_Variable, sep = "\n")
  ) %>%
  filter(Variable_Standard %in% c("FLFP", "CPR", "TFR"))

# Create significance heatmap
sig_heatmap <- ggplot(sig_data, aes(x = Variable_Standard, y = Model_Full, fill = Sig_Numeric)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = Sig_Level), color = "white", size = 4, fontface = "bold") +
  scale_fill_gradient2(
    low = "#f7f7f7", 
    mid = "#fed976", 
    high = "#fd8d3c",
    midpoint = 2,
    name = "Significance",
    labels = c("ns", "† p<0.10", "* p<0.05", "** p<0.01", "*** p<0.001"),
    breaks = c(0, 1, 2, 3, 4)
  ) +
  labs(
    title = "Statistical Significance Patterns Across Robustness Checks",
    subtitle = "Significance levels for key variables across different model specifications",
    x = "Variable",
    y = "Model Specification"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "right",
    panel.grid = element_blank()
  )

ggsave("significance_pattern_heatmap.png", sig_heatmap, width = 10, height = 12, dpi = 300)
cat("Saved: significance_pattern_heatmap.png\n")

# =============================================================================
# 4. ADDITIONAL SUMMARY VISUALIZATION
# =============================================================================

cat("\nCreating additional summary visualizations...\n")

# Effect size comparison (standardized coefficients)
effect_size_data <- coef_data %>%
  group_by(Variable_Standard, Dep_Variable) %>%
  summarise(
    Mean_Coef = mean(Coefficient, na.rm = TRUE),
    SD_Coef = sd(Coefficient, na.rm = TRUE),
    Min_Coef = min(Coefficient, na.rm = TRUE),
    Max_Coef = max(Coefficient, na.rm = TRUE),
    Range_Coef = Max_Coef - Min_Coef,
    N_Models = n(),
    .groups = "drop"
  )

effect_range_plot <- ggplot(effect_size_data, aes(x = Variable_Standard, y = Mean_Coef, color = Dep_Variable)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Min_Coef, ymax = Max_Coef), width = 0.2, alpha = 0.7) +
  facet_wrap(~Dep_Variable, scales = "free_y", ncol = 2) +
  scale_color_viridis_d(guide = "none") +
  labs(
    title = "Effect Size Ranges Across Robustness Checks",
    subtitle = "Points show mean coefficients, error bars show min-max range across specifications",
    x = "Variable",
    y = "Coefficient Estimate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("effect_size_ranges.png", effect_range_plot, width = 12, height = 10, dpi = 300)
cat("Saved: effect_size_ranges.png\n")

# =============================================================================
# 5. SAVE SUMMARY DATA
# =============================================================================

# Save processed data for further analysis
write_csv(coef_data, "robustness_coefficient_data.csv")
write_csv(r2_data, "robustness_r2_data.csv")  
write_csv(sig_data, "robustness_significance_data.csv")
write_csv(effect_size_data, "robustness_effect_size_summary.csv")

cat("\n=== ROBUSTNESS VISUALIZATIONS COMPLETED ===\n")
cat("Files created:\n")
cat("1. Individual coefficient plots for each dependent variable\n")
cat("2. combined_coefficient_robustness.png - All coefficient comparisons\n")
cat("3. model_performance_comparison.png - R² comparison across models\n")
cat("4. significance_pattern_heatmap.png - Significance patterns\n")
cat("5. effect_size_ranges.png - Effect size variability\n")
cat("6. CSV files with processed data for further analysis\n")

cat("\n=== INTERPRETATION NOTES ===\n")
cat("- Coefficient plots show robustness of effect sizes across specifications\n")
cat("- R² comparison indicates model fit stability\n")
cat("- Significance heatmap reveals which effects are consistently significant\n")
cat("- Effect size ranges show variability in estimates across robustness checks\n")
cat("- Red stars in coefficient plots indicate statistical significance (p<0.05)\n")