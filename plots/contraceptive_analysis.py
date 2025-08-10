import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.backends.backend_pdf import PdfPages

# Load the data
print("Loading and processing data...")
df = pd.read_csv('Contraceptive prevalence rate.csv')

# Clean the data - select relevant columns
df_clean = df[['Location', 'Time', 'Value']].copy()
df_clean = df_clean.dropna()

# Convert Time to int and Value to float
df_clean['Time'] = pd.to_numeric(df_clean['Time'], errors='coerce')
df_clean['Value'] = pd.to_numeric(df_clean['Value'], errors='coerce')
df_clean = df_clean.dropna()

# Sort values for better plotting
df_clean = df_clean.sort_values(['Location', 'Time'])

# Print some info for debugging
print(f"Data shape: {df_clean.shape}")
print(f"Countries: {df_clean['Location'].nunique()}")
print(f"Years range: {df_clean['Time'].min()} - {df_clean['Time'].max()}")
print(f"Value range: {df_clean['Value'].min():.1f} - {df_clean['Value'].max():.1f}")

# Create PDF file
with PdfPages('contraceptive_prevalence.pdf') as pdf:
    
    # 1. Historical trend for all available countries
    plt.figure(figsize=(14, 8))
    colors = plt.cm.Set3(np.linspace(0, 1, len(df_clean['Location'].unique())))
    
    for i, country in enumerate(df_clean['Location'].unique()):
        country_data = df_clean[df_clean['Location'] == country]
        plt.plot(country_data['Time'], country_data['Value'], 
                marker='o', linewidth=1, markersize=2, label=country, color=colors[i], alpha=0.7)
    
    plt.title('Contraceptive Prevalence Rate - All Countries (1990-2030)', 
              fontsize=16, fontweight='bold', pad=20)
    plt.xlabel('Year', fontsize=12)
    plt.ylabel('Contraceptive Prevalence Rate (%)', fontsize=12)
    plt.grid(True, alpha=0.3)
    plt.legend(bbox_to_anchor=(1.05, 1), loc='upper left', fontsize=8)
    plt.tight_layout()
    pdf.savefig()
    plt.close()
    
    # 2. Individual plots for each country
    countries = df_clean['Location'].unique()
    n_countries = len(countries)
    
    # Calculate subplot layout
    cols = 3
    rows = (n_countries + cols - 1) // cols
    
    fig, axes = plt.subplots(rows, cols, figsize=(15, 5*rows))
    if rows == 1:
        axes = axes.reshape(1, -1)
    
    for i, country in enumerate(countries):
        row = i // cols
        col = i % cols
        ax = axes[row, col]
        
        country_data = df_clean[df_clean['Location'] == country]
        ax.plot(country_data['Time'], country_data['Value'], 
               marker='o', linewidth=2, markersize=4, color='steelblue')
        
        ax.set_title(f'{country}', fontsize=12, fontweight='bold')
        ax.set_xlabel('Year', fontsize=10)
        ax.set_ylabel('Prevalence Rate (%)', fontsize=10)
        ax.grid(True, alpha=0.3)
        ax.tick_params(axis='both', which='major', labelsize=9)
        
        # Add value annotations for start and end years
        if len(country_data) > 0:
            start_val = country_data.iloc[0]['Value']
            end_val = country_data.iloc[-1]['Value']
            ax.annotate(f'{start_val:.1f}%', 
                       xy=(country_data.iloc[0]['Time'], start_val),
                       xytext=(5, 5), textcoords='offset points',
                       fontsize=8, ha='left')
            ax.annotate(f'{end_val:.1f}%', 
                       xy=(country_data.iloc[-1]['Time'], end_val),
                       xytext=(5, 5), textcoords='offset points',
                       fontsize=8, ha='left')
    
    # Hide empty subplots
    for i in range(n_countries, rows * cols):
        row = i // cols
        col = i % cols
        axes[row, col].set_visible(False)
    
    plt.suptitle('Contraceptive Prevalence Rate by Country (1990-2030)', 
                fontsize=16, fontweight='bold', y=0.98)
    plt.tight_layout()
    pdf.savefig()
    plt.close()

print("Analysis complete! PDF saved as 'contraceptive_prevalence.pdf'")
print(f"Created plots for {n_countries} countries") 