import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.backends.backend_pdf import PdfPages

# Load the data
print("Loading and processing data...")
df = pd.read_csv('Female labor force participation rate.csv', skiprows=4)  # Skip header rows

# Clean the data - remove rows with all NaN values and select relevant columns
df_clean = df.dropna(how='all')
df_clean = df_clean[df_clean['Country Name'].notna()]

# Convert from wide to long format
year_columns = [col for col in df_clean.columns if col.isdigit()]
df_long = df_clean.melt(
    id_vars=['Country Name', 'Country Code', 'Indicator Name', 'Indicator Code'],
    value_vars=year_columns,
    var_name='Year',
    value_name='Participation_Rate'
)

# Convert Year to int and Participation_Rate to float
df_long['Year'] = pd.to_numeric(df_long['Year'], errors='coerce')
df_long['Participation_Rate'] = pd.to_numeric(df_long['Participation_Rate'], errors='coerce')

# Drop rows with missing values
df_long = df_long.dropna(subset=['Year', 'Participation_Rate', 'Country Name'])

# Sort the data
df_long = df_long.sort_values(['Country Name', 'Year'])

# Filter for countries with sufficient data (at least 10 data points)
country_counts = df_long.groupby('Country Name').size()
countries_with_data = country_counts[country_counts >= 10].index
df_filtered = df_long[df_long['Country Name'].isin(countries_with_data)]

print(f"Number of countries with sufficient data: {len(countries_with_data)}")
print(f"Year range: {df_filtered['Year'].min()} - {df_filtered['Year'].max()}")
print(f"Total data points: {len(df_filtered)}")

# Create PDF file
with PdfPages('female_labor_force_participation.pdf') as pdf:
    
    # 1. Historical trend for all available countries
    plt.figure(figsize=(16, 10))
    colors = plt.cm.Set3(np.linspace(0, 1, len(countries_with_data)))
    
    for i, country in enumerate(sorted(countries_with_data)):
        country_data = df_filtered[df_filtered['Country Name'] == country]
        plt.plot(country_data['Year'], country_data['Participation_Rate'], 
                 marker='o', linewidth=1, markersize=2, label=country, color=colors[i], alpha=0.7)
    
    plt.title('Female Labor Force Participation Rate - All Countries (1990-2024)', 
              fontsize=16, fontweight='bold', pad=20)
    plt.xlabel('Year', fontsize=12)
    plt.ylabel('Participation Rate (% of female population ages 15+)', fontsize=12)
    plt.legend(bbox_to_anchor=(1.05, 1), loc='upper left', fontsize=8, ncol=2)
    plt.grid(True, alpha=0.3)
    plt.tight_layout()
    pdf.savefig()
    plt.close()
    
    # 2. Historical trend for Germany, France, South Korea and Greece
    plt.figure(figsize=(12, 8))
    selected_countries = ['Germany', 'France', 'Korea, Rep.', 'Greece']
    colors_selected = ['#1f77b4', '#ff7f0e', '#2ca02c', '#d62728']
    
    for i, country in enumerate(selected_countries):
        country_data = df_filtered[df_filtered['Country Name'] == country]
        if len(country_data) > 0:
            plt.plot(country_data['Year'], country_data['Participation_Rate'], 
                     marker='o', linewidth=3, markersize=6, label=country, color=colors_selected[i])
    
    plt.title('Female Labor Force Participation Rate - Selected Countries (1990-2024)', 
              fontsize=16, fontweight='bold', pad=20)
    plt.xlabel('Year', fontsize=12)
    plt.ylabel('Participation Rate (% of female population ages 15+)', fontsize=12)
    plt.legend(fontsize=12)
    plt.grid(True, alpha=0.3)
    plt.tight_layout()
    pdf.savefig()
    plt.close()
    
    # 3. Individual plots for each country (top 20 countries by data availability)
    top_countries = country_counts.nlargest(20).index
    n_countries = len(top_countries)
    
    # Calculate subplot layout
    cols = 4
    rows = (n_countries + cols - 1) // cols
    
    fig, axes = plt.subplots(rows, cols, figsize=(20, 5*rows))
    fig.suptitle('Female Labor Force Participation Rate by Country (1960-2024)', 
                 fontsize=16, fontweight='bold', y=0.98)
    
    # Flatten axes for easier indexing
    if rows == 1:
        axes = [axes] if cols == 1 else axes
    else:
        axes = axes.flatten()
    
    for i, country in enumerate(top_countries):
        country_data = df_filtered[df_filtered['Country Name'] == country]
        
        axes[i].plot(country_data['Year'], country_data['Participation_Rate'], 
                     marker='o', linewidth=2, markersize=3, color='#1f77b4')
        axes[i].set_title(f'{country}', fontsize=10, fontweight='bold')
        axes[i].set_xlabel('Year', fontsize=8)
        axes[i].set_ylabel('Participation Rate (%)', fontsize=8)
        axes[i].grid(True, alpha=0.3)
        axes[i].tick_params(axis='both', which='major', labelsize=7)
        
        # Add some statistics as text
        mean_rate = country_data['Participation_Rate'].mean()
        min_rate = country_data['Participation_Rate'].min()
        max_rate = country_data['Participation_Rate'].max()
        axes[i].text(0.02, 0.98, f'Mean: {mean_rate:.1f}%\nMin: {min_rate:.1f}%\nMax: {max_rate:.1f}%', 
                     transform=axes[i].transAxes, verticalalignment='top', 
                     bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.8), fontsize=7)
    
    # Hide empty subplots
    for i in range(n_countries, len(axes)):
        axes[i].set_visible(False)
    
    plt.tight_layout()
    pdf.savefig()
    plt.close()

print("PDF file 'female_labor_force_participation.pdf' has been created successfully!")
print(f"Contains data for {len(countries_with_data)} countries from {df_filtered['Year'].min()} to {df_filtered['Year'].max()}")

# Print summary statistics for selected countries
selected_countries = ['Germany', 'France', 'Korea, Rep.', 'Greece']
print("\nSummary statistics for selected countries:")
for country in selected_countries:
    country_data = df_filtered[df_filtered['Country Name'] == country]
    if len(country_data) > 0:
        print(f"\n{country}:")
        print(f"  Data points: {len(country_data)}")
        print(f"  Year range: {country_data['Year'].min()} - {country_data['Year'].max()}")
        print(f"  Mean participation rate: {country_data['Participation_Rate'].mean():.1f}%")
        print(f"  Range: {country_data['Participation_Rate'].min():.1f}% - {country_data['Participation_Rate'].max():.1f}%") 

selected_countries = [
    'Japan', 'Italy', 'Greece', 'Germany', 'France', 'Chile', 'Brazil',
    'Korea, Rep.', 'Spain', 'Sweden', 'Mexico'
]

# Filter for selected countries only
df_selected = df_long[df_long['Country Name'].isin(selected_countries)]

print(f"Number of selected countries: {len(selected_countries)}")
print(f"Year range: {df_selected['Year'].min()} - {df_selected['Year'].max()}")
print(f"Total data points: {len(df_selected)}")

with PdfPages('female_labor_force_participation_selected.pdf') as pdf:
    # 1. Historical trend for all selected countries
    plt.figure(figsize=(16, 10))
    colors = plt.cm.Set3(np.linspace(0, 1, len(selected_countries)))
    for i, country in enumerate(selected_countries):
        country_data = df_selected[df_selected['Country Name'] == country]
        if len(country_data) > 0:
            plt.plot(country_data['Year'], country_data['Participation_Rate'], 
                     marker='o', linewidth=1, markersize=2, label=country, color=colors[i], alpha=0.7)
    plt.title('Female Labor Force Participation Rate - Selected Countries (1990-2024)', 
              fontsize=16, fontweight='bold', pad=20)
    plt.xlabel('Year', fontsize=12)
    plt.ylabel('Participation Rate (% of female population ages 15+)', fontsize=12)
    plt.legend(bbox_to_anchor=(1.05, 1), loc='upper left', fontsize=8, ncol=2)
    plt.grid(True, alpha=0.3)
    plt.tight_layout()
    pdf.savefig()
    plt.close()

    # 2. Historical trend for 4 countries (Germany, France, Korea, Greece)
    plt.figure(figsize=(12, 8))
    subset_countries = ['Germany', 'France', 'Korea, Rep.', 'Greece']
    colors_selected = ['#1f77b4', '#ff7f0e', '#2ca02c', '#d62728']
    for i, country in enumerate(subset_countries):
        country_data = df_selected[df_selected['Country Name'] == country]
        if len(country_data) > 0:
            plt.plot(country_data['Year'], country_data['Participation_Rate'], 
                     marker='o', linewidth=3, markersize=6, label=country, color=colors_selected[i])
    plt.title('Female Labor Force Participation Rate - Subset (1990-2024)', 
              fontsize=16, fontweight='bold', pad=20)
    plt.xlabel('Year', fontsize=12)
    plt.ylabel('Participation Rate (% of female population ages 15+)', fontsize=12)
    plt.legend(fontsize=12)
    plt.grid(True, alpha=0.3)
    plt.tight_layout()
    pdf.savefig()
    plt.close()

    # 3. Individual plots for each selected country
    n_countries = len(selected_countries)
    cols = 4
    rows = (n_countries + cols - 1) // cols
    fig, axes = plt.subplots(rows, cols, figsize=(20, 5*rows))
    fig.suptitle('Female Labor Force Participation Rate by Country (1990-2024)', 
                 fontsize=16, fontweight='bold', y=0.98)
    if rows == 1:
        axes = [axes] if cols == 1 else axes
    else:
        axes = axes.flatten()
    for i, country in enumerate(selected_countries):
        country_data = df_selected[df_selected['Country Name'] == country]
        axes[i].plot(country_data['Year'], country_data['Participation_Rate'], 
                     marker='o', linewidth=2, markersize=3, color='#1f77b4')
        axes[i].set_title(f'{country}', fontsize=10, fontweight='bold')
        axes[i].set_xlabel('Year', fontsize=8)
        axes[i].set_ylabel('Participation Rate (%)', fontsize=8)
        axes[i].grid(True, alpha=0.3)
        axes[i].tick_params(axis='both', which='major', labelsize=7)
        mean_rate = country_data['Participation_Rate'].mean()
        min_rate = country_data['Participation_Rate'].min()
        max_rate = country_data['Participation_Rate'].max()
        axes[i].text(0.02, 0.98, f'Mean: {mean_rate:.1f}%\nMin: {min_rate:.1f}%\nMax: {max_rate:.1f}%', 
                     transform=axes[i].transAxes, verticalalignment='top', 
                     bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.8), fontsize=7)
    for i in range(n_countries, len(axes)):
        axes[i].set_visible(False)
    plt.tight_layout()
    pdf.savefig()
    plt.close()

print("PDF file 'female_labor_force_participation_selected.pdf' has been created successfully!")
print(f"Contains data for {len(selected_countries)} countries from {df_selected['Year'].min()} to {df_selected['Year'].max()}")

# Print summary statistics for selected countries
print("\nSummary statistics for selected countries:")
for country in selected_countries:
    country_data = df_selected[df_selected['Country Name'] == country]
    if len(country_data) > 0:
        print(f"\n{country}:")
        print(f"  Data points: {len(country_data)}")
        print(f"  Year range: {country_data['Year'].min()} - {country_data['Year'].max()}")
        print(f"  Mean participation rate: {country_data['Participation_Rate'].mean():.1f}%")
        print(f"  Range: {country_data['Participation_Rate'].min():.1f}% - {country_data['Participation_Rate'].max():.1f}%") 