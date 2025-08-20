import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.backends.backend_pdf import PdfPages

# Load and clean the data
print("Loading and processing data...")
df = pd.read_csv('Fertility Rates.csv')
df_clean = df[['Country', 'TIME_PERIOD', 'OBS_VALUE']].copy()
df_clean = df_clean.dropna()
df_clean['TIME_PERIOD'] = pd.to_numeric(df_clean['TIME_PERIOD'], errors='coerce')
df_clean['OBS_VALUE'] = pd.to_numeric(df_clean['OBS_VALUE'], errors='coerce')
df_clean = df_clean.dropna()
df_clean = df_clean.sort_values(['Country', 'TIME_PERIOD'])

# Create PDF file
with PdfPages('fertility_rate.pdf') as pdf:
    
    # 1. Historical trend for all available countries
    plt.figure(figsize=(14, 8))
    colors = plt.cm.Set3(np.linspace(0, 1, len(df_clean['Country'].unique())))
    
    for i, country in enumerate(sorted(df_clean['Country'].unique())):
        country_data = df_clean[df_clean['Country'] == country]
        plt.plot(country_data['TIME_PERIOD'], country_data['OBS_VALUE'], 
                 marker='o', linewidth=2, markersize=4, label=country, color=colors[i])
    
    plt.title('Fertility Rate Historical Trend - All Countries (1990-2021)', 
              fontsize=16, fontweight='bold', pad=20)
    plt.xlabel('Year', fontsize=12)
    plt.ylabel('Fertility Rate (Children per Woman)', fontsize=12)
    plt.legend(bbox_to_anchor=(1.05, 1), loc='upper left', fontsize=10)
    plt.grid(True, alpha=0.3)
    plt.tight_layout()
    pdf.savefig()
    plt.close()
    
    # 2. Historical trend for Germany, France, South Korea and Greece
    plt.figure(figsize=(12, 8))
    selected_countries = ['Germany', 'France', 'Korea', 'Greece']
    colors_selected = ['#1f77b4', '#ff7f0e', '#2ca02c', '#d62728']
    
    for i, country in enumerate(selected_countries):
        country_data = df_clean[df_clean['Country'] == country]
        plt.plot(country_data['TIME_PERIOD'], country_data['OBS_VALUE'], 
                 marker='o', linewidth=3, markersize=6, label=country, color=colors_selected[i])
    
    plt.title('Fertility Rate Historical Trend - Selected Countries (1990-2021)', 
              fontsize=16, fontweight='bold', pad=20)
    plt.xlabel('Year', fontsize=12)
    plt.ylabel('Fertility Rate (Children per Woman)', fontsize=12)
    plt.legend(fontsize=12)
    plt.grid(True, alpha=0.3)
    plt.tight_layout()
    pdf.savefig()
    plt.close()
    
    # 3. Individual plots for each country
    countries = sorted(df_clean['Country'].unique())
    n_countries = len(countries)
    
    # Calculate subplot layout
    cols = 3
    rows = (n_countries + cols - 1) // cols
    
    fig, axes = plt.subplots(rows, cols, figsize=(18, 6*rows))
    fig.suptitle('Fertility Rate Historical Trend by Country (1990-2021)', 
                 fontsize=16, fontweight='bold', y=0.98)
    
    # Flatten axes for easier indexing
    if rows == 1:
        axes = [axes] if cols == 1 else axes
    else:
        axes = axes.flatten()
    
    for i, country in enumerate(countries):
        country_data = df_clean[df_clean['Country'] == country]
        
        axes[i].plot(country_data['TIME_PERIOD'], country_data['OBS_VALUE'], 
                     marker='o', linewidth=2, markersize=4, color='#1f77b4')
        axes[i].set_title(f'{country}', fontsize=12, fontweight='bold')
        axes[i].set_xlabel('Year', fontsize=10)
        axes[i].set_ylabel('Fertility Rate', fontsize=10)
        axes[i].grid(True, alpha=0.3)
        axes[i].tick_params(axis='both', which='major', labelsize=9)
        
        # Add some statistics as text
        mean_rate = country_data['OBS_VALUE'].mean()
        min_rate = country_data['OBS_VALUE'].min()
        max_rate = country_data['OBS_VALUE'].max()
        axes[i].text(0.02, 0.98, f'Mean: {mean_rate:.2f}\nMin: {min_rate:.2f}\nMax: {max_rate:.2f}', 
                     transform=axes[i].transAxes, verticalalignment='top', 
                     bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.8), fontsize=8)
    
    # Hide empty subplots
    for i in range(n_countries, len(axes)):
        axes[i].set_visible(False)
    
    plt.tight_layout()
    pdf.savefig()
    plt.close()

print("PDF file 'fertility_rate.pdf' has been created successfully!")
print(f"Contains {n_countries} countries with data from {df_clean['TIME_PERIOD'].min()} to {df_clean['TIME_PERIOD'].max()}")
print("\nSummary statistics:")
summary = df_clean.groupby('Country')['OBS_VALUE'].agg(['mean', 'min', 'max', 'count']).round(2)
print(summary) 