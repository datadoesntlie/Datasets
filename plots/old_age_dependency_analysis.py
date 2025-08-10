import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.backends.backend_pdf import PdfPages

# Load the data
print("Loading and processing data...")
df = pd.read_csv('Old Age Dependancy Ratio.csv')

# Clean the data - select relevant columns and drop rows with missing values
df_clean = df[['Country', 'TIME_PERIOD', 'OBS_VALUE']].copy()
df_clean = df_clean.dropna()

# Convert TIME_PERIOD to int and OBS_VALUE to float
df_clean['TIME_PERIOD'] = pd.to_numeric(df_clean['TIME_PERIOD'], errors='coerce')
df_clean['OBS_VALUE'] = pd.to_numeric(df_clean['OBS_VALUE'], errors='coerce')
df_clean = df_clean.dropna()

# Sort values for better plotting
df_clean = df_clean.sort_values(['Country', 'TIME_PERIOD'])

# Print some info for debugging
print(f"Data shape: {df_clean.shape}")
print(f"Countries: {df_clean['Country'].nunique()}")
print(f"Years range: {df_clean['TIME_PERIOD'].min()} - {df_clean['TIME_PERIOD'].max()}")

# Create PDF file
with PdfPages('old_age_dependency_trend.pdf') as pdf:
    plt.figure(figsize=(14, 8))
    colors = plt.cm.Set3(np.linspace(0, 1, len(df_clean['Country'].unique())))
    
    for i, country in enumerate(df_clean['Country'].unique()):
        country_data = df_clean[df_clean['Country'] == country]
        plt.plot(country_data['TIME_PERIOD'], country_data['OBS_VALUE'], 
                 marker='o', linewidth=2, markersize=4, label=country, color=colors[i], alpha=0.8)
    
    plt.title('Old-age Dependency Ratio - All Countries', fontsize=16, fontweight='bold', pad=20)
    plt.xlabel('Year', fontsize=12)
    plt.ylabel('Old-age Dependency Ratio (%)', fontsize=12)
    plt.grid(True, alpha=0.3)
    plt.legend(bbox_to_anchor=(1.05, 1), loc='upper left', fontsize=8)
    plt.tight_layout()
    pdf.savefig()
    plt.close()

print("Analysis complete! PDF saved as 'old_age_dependency_trend.pdf'") 