import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.backends.backend_pdf import PdfPages
from scipy import stats
import seaborn as sns

# Load both datasets
print("Loading and processing data...")

# Load contraceptive prevalence data
contraceptive_df = pd.read_csv('Contraceptive prevalence rate.csv')
contraceptive_clean = contraceptive_df[['Location', 'Time', 'Value']].copy()
contraceptive_clean = contraceptive_clean.dropna()
contraceptive_clean['Time'] = pd.to_numeric(contraceptive_clean['Time'], errors='coerce')
contraceptive_clean['Value'] = pd.to_numeric(contraceptive_clean['Value'], errors='coerce')
contraceptive_clean = contraceptive_clean.dropna()
contraceptive_clean = contraceptive_clean.rename(columns={'Location': 'Country', 'Value': 'Contraceptive_Rate'})

# Load fertility rate data
fertility_df = pd.read_csv('Fertility Rates.csv')
fertility_clean = fertility_df[['Country', 'TIME_PERIOD', 'OBS_VALUE']].copy()
fertility_clean = fertility_clean.dropna()
fertility_clean['TIME_PERIOD'] = pd.to_numeric(fertility_clean['TIME_PERIOD'], errors='coerce')
fertility_clean['OBS_VALUE'] = pd.to_numeric(fertility_clean['OBS_VALUE'], errors='coerce')
fertility_clean = fertility_clean.dropna()
fertility_clean = fertility_clean.rename(columns={'TIME_PERIOD': 'Time', 'OBS_VALUE': 'Fertility_Rate'})

# Merge the datasets
merged_df = pd.merge(contraceptive_clean, fertility_clean, on=['Country', 'Time'], how='inner')
merged_df = merged_df.sort_values(['Country', 'Time'])

print(f"Merged data shape: {merged_df.shape}")
print(f"Countries with both datasets: {merged_df['Country'].nunique()}")
print(f"Years range: {merged_df['Time'].min()} - {merged_df['Time'].max()}")

# Calculate overall correlation
overall_corr, overall_p_value = stats.pearsonr(merged_df['Contraceptive_Rate'], merged_df['Fertility_Rate'])

# Calculate correlation by country
country_correlations = []
for country in merged_df['Country'].unique():
    country_data = merged_df[merged_df['Country'] == country]
    if len(country_data) > 3:  # Need at least 4 points for correlation
        corr, p_value = stats.pearsonr(country_data['Contraceptive_Rate'], country_data['Fertility_Rate'])
        country_correlations.append({
            'Country': country,
            'Correlation': corr,
            'P_Value': p_value,
            'Data_Points': len(country_data)
        })

correlation_df = pd.DataFrame(country_correlations)
correlation_df = correlation_df.sort_values('Correlation', ascending=False)

# Create PDF file
with PdfPages('fertility_contraceptive_correlation.pdf') as pdf:
    
    # 1. Overall correlation scatter plot
    plt.figure(figsize=(12, 8))
    plt.scatter(merged_df['Contraceptive_Rate'], merged_df['Fertility_Rate'], 
               alpha=0.6, s=30, color='steelblue')
    
    # Add trend line
    z = np.polyfit(merged_df['Contraceptive_Rate'], merged_df['Fertility_Rate'], 1)
    p = np.poly1d(z)
    plt.plot(merged_df['Contraceptive_Rate'], p(merged_df['Contraceptive_Rate']), 
            "r--", alpha=0.8, linewidth=2)
    
    plt.title(f'Fertility Rate vs Contraceptive Prevalence Rate\nOverall Correlation: {overall_corr:.3f} (p={overall_p_value:.3e})', 
              fontsize=16, fontweight='bold', pad=20)
    plt.xlabel('Contraceptive Prevalence Rate (%)', fontsize=12)
    plt.ylabel('Fertility Rate (children per woman)', fontsize=12)
    plt.grid(True, alpha=0.3)
    plt.tight_layout()
    pdf.savefig()
    plt.close()
    
    # 2. Correlation summary table
    fig, ax = plt.subplots(figsize=(12, 8))
    ax.axis('tight')
    ax.axis('off')
    
    # Prepare table data
    table_data = []
    for _, row in correlation_df.iterrows():
        significance = "***" if row['P_Value'] < 0.001 else "**" if row['P_Value'] < 0.01 else "*" if row['P_Value'] < 0.05 else ""
        table_data.append([
            row['Country'],
            f"{row['Correlation']:.3f}{significance}",
            f"{row['P_Value']:.3e}",
            row['Data_Points']
        ])
    
    table = ax.table(cellText=table_data,
                    colLabels=['Country', 'Correlation', 'P-Value', 'Data Points'],
                    cellLoc='center',
                    loc='center',
                    colWidths=[0.3, 0.2, 0.2, 0.15])
    
    table.auto_set_font_size(False)
    table.set_fontsize(10)
    table.scale(1.2, 1.5)
    
    # Style the table
    for i in range(len(table_data) + 1):
        for j in range(4):
            if i == 0:  # Header row
                table[(i, j)].set_facecolor('#4CAF50')
                table[(i, j)].set_text_props(weight='bold', color='white')
            else:
                table[(i, j)].set_facecolor('#f0f0f0' if i % 2 == 0 else 'white')
    
    plt.title('Correlation Analysis by Country\nFertility Rate vs Contraceptive Prevalence Rate', 
              fontsize=16, fontweight='bold', pad=20)
    plt.tight_layout()
    pdf.savefig()
    plt.close()
    
    # 3. Individual country plots with both variables
    countries = merged_df['Country'].unique()
    n_countries = len(countries)
    
    # Calculate subplot layout
    cols = 3
    rows = (n_countries + cols - 1) // cols
    
    fig, axes = plt.subplots(rows, cols, figsize=(18, 6*rows))
    if rows == 1:
        axes = axes.reshape(1, -1)
    
    for i, country in enumerate(countries):
        row = i // cols
        col = i % cols
        ax = axes[row, col]
        
        country_data = merged_df[merged_df['Country'] == country]
        
        # Create twin axes for two y-axes
        ax2 = ax.twinx()
        
        # Plot contraceptive rate on left y-axis
        line1 = ax.plot(country_data['Time'], country_data['Contraceptive_Rate'], 
                       marker='o', linewidth=2, markersize=4, color='steelblue', label='Contraceptive Rate')
        ax.set_ylabel('Contraceptive Rate (%)', color='steelblue', fontsize=10)
        ax.tick_params(axis='y', labelcolor='steelblue')
        
        # Plot fertility rate on right y-axis
        line2 = ax2.plot(country_data['Time'], country_data['Fertility_Rate'], 
                        marker='s', linewidth=2, markersize=4, color='red', label='Fertility Rate')
        ax2.set_ylabel('Fertility Rate', color='red', fontsize=10)
        ax2.tick_params(axis='y', labelcolor='red')
        
        # Get correlation for this country
        country_corr = correlation_df[correlation_df['Country'] == country]['Correlation'].iloc[0]
        country_p = correlation_df[correlation_df['Country'] == country]['P_Value'].iloc[0]
        
        significance = "***" if country_p < 0.001 else "**" if country_p < 0.01 else "*" if country_p < 0.05 else ""
        ax.set_title(f'{country}\nCorr: {country_corr:.3f}{significance}', 
                    fontsize=12, fontweight='bold')
        ax.set_xlabel('Year', fontsize=10)
        ax.grid(True, alpha=0.3)
        ax.tick_params(axis='both', which='major', labelsize=9)
        
        # Add legend
        lines = line1 + line2
        labels = [l.get_label() for l in lines]
        ax.legend(lines, labels, loc='upper right', fontsize=8)
    
    # Hide empty subplots
    for i in range(n_countries, rows * cols):
        row = i // cols
        col = i % cols
        axes[row, col].set_visible(False)
    
    plt.suptitle('Fertility Rate vs Contraceptive Prevalence Rate by Country (1990-2030)', 
                fontsize=16, fontweight='bold', y=0.98)
    plt.tight_layout()
    pdf.savefig()
    plt.close()
    
    # 4. Correlation heatmap
    plt.figure(figsize=(10, 8))
    
    # Create correlation matrix for visualization
    corr_matrix = correlation_df[['Country', 'Correlation']].set_index('Country')
    
    # Create heatmap
    sns.heatmap(corr_matrix.T, annot=True, cmap='RdBu_r', center=0, 
                fmt='.3f', cbar_kws={'label': 'Correlation Coefficient'})
    
    plt.title('Correlation Coefficients by Country\nFertility Rate vs Contraceptive Prevalence Rate', 
              fontsize=16, fontweight='bold', pad=20)
    plt.tight_layout()
    pdf.savefig()
    plt.close()

print("Analysis complete! PDF saved as 'fertility_contraceptive_correlation.pdf'")
print(f"Overall correlation: {overall_corr:.3f} (p={overall_p_value:.3e})")
print(f"Analyzed {len(correlation_df)} countries")
print("\nTop 5 positive correlations:")
print(correlation_df.head().to_string(index=False))
print("\nTop 5 negative correlations:")
print(correlation_df.tail().to_string(index=False)) 