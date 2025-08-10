# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains data and analysis scripts for an econometric study titled "The Double-Edged Contribution: Female Labor Participation and Fertility in PAYG Pension Sustainability". The research examines how female labor force participation (FLFP) and contraceptive access affect Pay-As-You-Go pension system sustainability using panel data econometric methods.

## Repository Structure

- **Root directory**: Contains CSV files with raw datasets for all variables
- **plots/**: Python scripts for data visualization and descriptive analysis (completed for literature review - reference only)
- **analysis/**: Generated PDF visualizations from the plots scripts
- **visualizations/**: Additional analysis outputs
- **plotsthesis/**: Python virtual environment (Python 3.13.3) - legacy from literature review phase
- **R/**: R scripts for econometric analysis and publication-ready visualizations (to be created)

## Research Variables

### Dependent Variables (Pension Sustainability)
- Public pension expenditure (% GDP)
- Old-age dependency ratio
- Pension financing gap = Pension expenditure − Social Security Contributions (% GDP)
- Social Security Contributions (% GDP)

### Key Independent Variables
- Female labor force participation (% of working-age women)
- Contraceptive prevalence rate (CPR, % of women using modern methods)
- Total fertility rate (TFR)

### Control Variables
- GDP per capita (constant USD)
- Life expectancy at age 65
- Female tertiary education rate (% of women 25+)
- Urbanization rate (% living in urban areas)

## Current Analysis Phase: Data and Descriptive Statistics

### Summary Statistics Tasks
1. **Comprehensive Summary Table**: Create one table with all variables including mean, std dev, min, max, observations
2. **Separate Tables**: Consider developed vs developing countries comparison
3. **Handle Missing Data**: Use linear interpolation for 1-2 year gaps, report systematic patterns

### Data Visualization Requirements (Section 3.4)

#### Essential Visualizations (4-6 focused plots):
1. **Time Trends** (2-3 plots):
   - FLFP trends over time by country (line graph)
   - Pension expenditure trends over time by country
   - CPR and TFR trends (dual y-axis plot)

2. **Cross-Country Comparisons** (1-2 plots):
   - Scatter: FLFP vs Pension Expenditure (with country labels)
   - Bar chart: Average values by country for key variables

3. **Correlation Analysis**:
   - Correlation matrix heatmap of all main variables

4. **Key Relationships** (1-2 plots):
   - Scatter: CPR vs TFR (contraception-fertility relationship)
   - Box plots: Pension sustainability by country groups

## Development Environment

### R Environment (Primary for Econometric Analysis)
- **Key Packages for Analysis**: 
  - Data manipulation: `dplyr`, `tidyr`, `readr`
  - Visualization: `ggplot2`, `corrplot`, `gridExtra`
  - Panel data econometrics: `plm`, `lmtest`, `sandwich`
  - Publication tables: `stargazer`, `kableExtra`, `modelsummary`
  - Time series: `tseries`, `forecast`
- **Installation**: Use `install.packages()` for CRAN packages
- **Data Export**: Use `ggsave()` for high-quality plots, `pdf()` for multi-page outputs

### Python Environment (Legacy - Literature Review Reference)
- **Virtual Environment**: `plotsthesis/` (Python 3.13.3) 
- **Status**: Reference only for completed literature review visualizations
- **Key Libraries**: pandas, matplotlib, numpy, scipy, seaborn

### Common R Data Processing Patterns
- Use `read.csv(skip=4)` for World Bank formatted files
- Apply country name standardization with `case_when()` or named vectors
- Convert wide to long format using `pivot_longer()` from tidyr
- Use `ggsave()` or `pdf()` for publication-ready outputs
- Handle missing data with `na.approx()` from zoo package for interpolation

### Econometric Model (for reference)
```
PensionSustainabilityit = β0 + β1FLFPit + β2CPRit + β3TFRit + β4GDPpcit + β5LifeExpect65it + β6Eduit + β7Urbanit + αi + εit
```

## Data Limitations to Consider
- Sample: Only 11 countries (limits generalizability)
- Brazil data starts from 2004 (unbalanced panel)
- Some systematic missing data patterns
- Potential endogeneity between FLFP and fertility

## Expected Analysis Pipeline (R-based)
1. **Data Cleaning**: Standardize country names, handle missing values using R/dplyr
2. **Descriptive Statistics**: Summary tables using stargazer/kableExtra, correlation analysis
3. **Visualization**: Time trends, cross-country comparisons, relationships using ggplot2
4. **Panel Data Setup**: Country-time structure using plm, stationarity testing with tseries
5. **Econometric Estimation**: Fixed/Random effects models with plm, robustness checks with lmtest/sandwich
6. **Publication Tables**: Model results using stargazer for journal-ready output

## Key Research Hypotheses
- H1: Higher FLFP has mixed effects on pension sustainability
- H2: Greater contraceptive access worsens sustainability through lower fertility  
- H3: Effects differ between developed and developing countries