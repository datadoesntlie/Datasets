import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.backends.backend_pdf import PdfPages
from scipy.stats import pearsonr
import seaborn as sns

# --- Load and process Female Labor Force Participation Rate data ---
print("Loading and processing female labor force participation data...")
labor_df = pd.read_csv('Female labor force participation rate.csv', skiprows=4)
labor_df = labor_df.dropna(subset=['Country Name'])

# Create country name mapping to standardize names between datasets
country_mapping = {
    'Korea, Rep.': 'Korea',
    'Korea, Republic of': 'Korea',
    'United States': 'United States',
    'United Kingdom': 'United Kingdom',
    # Add more mappings as needed
}

# Apply country name mapping to labor force data
labor_df['Country Name'] = labor_df['Country Name'].replace(country_mapping)

year_columns = [col for col in labor_df.columns if col.isdigit()]
labor_long = labor_df.melt(
    id_vars=['Country Name', 'Country Code'],
    value_vars=year_columns,
    var_name='Year',
    value_name='LaborForceRate'
)
labor_long['Year'] = pd.to_numeric(labor_long['Year'], errors='coerce')
labor_long['LaborForceRate'] = pd.to_numeric(labor_long['LaborForceRate'], errors='coerce')
labor_long = labor_long.dropna(subset=['LaborForceRate'])

# --- Load and process Fertility Rate data ---
print("Loading and processing fertility rate data...")
fertility_df = pd.read_csv('Fertility Rates.csv')
fertility_clean = fertility_df[['Country', 'TIME_PERIOD', 'OBS_VALUE']].copy()
fertility_clean = fertility_clean.dropna()
fertility_clean['TIME_PERIOD'] = pd.to_numeric(fertility_clean['TIME_PERIOD'], errors='coerce')
fertility_clean['OBS_VALUE'] = pd.to_numeric(fertility_clean['OBS_VALUE'], errors='coerce')
fertility_clean = fertility_clean.dropna()
fertility_clean = fertility_clean.rename(columns={'Country': 'Country Name', 'TIME_PERIOD': 'Year', 'OBS_VALUE': 'FertilityRate'})

# --- Merge datasets on Country and Year ---
print("Merging datasets...")
merged = pd.merge(labor_long, fertility_clean, on=['Country Name', 'Year'])

# Print some debugging info
print(f"Countries in merged dataset: {sorted(merged['Country Name'].unique())}")
print(f"Total data points: {len(merged)}")

# --- Correlation analysis ---
print("Performing correlation analysis...")
correlations = []
country_list = merged['Country Name'].unique()

with PdfPages('female_labor_fertility_correlation.pdf') as pdf:
    for country in sorted(country_list):
        data = merged[merged['Country Name'] == country]
        if len(data) < 2:
            continue  # Not enough data for correlation
        corr, pval = pearsonr(data['LaborForceRate'], data['FertilityRate'])
        correlations.append({'Country': country, 'Correlation': corr, 'P-value': pval, 'N': len(data)})
        # Plot
        plt.figure(figsize=(7, 5))
        sns.regplot(x='LaborForceRate', y='FertilityRate', data=data, scatter_kws={'s': 30, 'alpha': 0.7})
        plt.title(f'{country}\nPearson r={corr:.2f}, p={pval:.3f}, N={len(data)}')
        plt.xlabel('Female Labor Force Participation Rate (%)')
        plt.ylabel('Fertility Rate (children per woman)')
        plt.tight_layout()
        pdf.savefig()
        plt.close()

    # Overall correlation (all data)
    if len(merged) > 2:
        corr_all, pval_all = pearsonr(merged['LaborForceRate'], merged['FertilityRate'])
        plt.figure(figsize=(8, 6))
        sns.regplot(x='LaborForceRate', y='FertilityRate', data=merged, scatter_kws={'s': 20, 'alpha': 0.5})
        plt.title(f'All Countries\nPearson r={corr_all:.2f}, p={pval_all:.3f}, N={len(merged)})')
        plt.xlabel('Female Labor Force Participation Rate (%)')
        plt.ylabel('Fertility Rate (children per woman)')
        plt.tight_layout()
        pdf.savefig()
        plt.close()

# --- Save correlation summary table ---
corr_df = pd.DataFrame(correlations)
corr_df = corr_df.sort_values('Correlation')
corr_df.to_csv('female_labor_fertility_correlation_summary.csv', index=False)

print('Analysis complete. PDF and summary CSV saved.') 