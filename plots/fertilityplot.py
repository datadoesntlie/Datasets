import pandas as pd
import matplotlib.pyplot as plt

# Load the data
df = pd.read_csv('Fertility Rates.csv')

# Select relevant columns and rename for convenience
df = df[['Country', 'Time', 'Observation Value']].rename(
    columns={'Country': 'Country', 'Time': 'Year', 'Observation Value': 'FertilityRate'}
)

# Drop rows with missing values in any of the relevant columns
df = df.dropna(subset=['Country', 'Year', 'FertilityRate'])

# Convert Year to int and FertilityRate to float
df['Year'] = df['Year'].astype(int)
df['FertilityRate'] = df['FertilityRate'].astype(float)

# Sort values for better plotting
df = df.sort_values(['Country', 'Year'])

# Plot
plt.figure(figsize=(12, 7))
for country in df['Country'].unique():
    country_data = df[df['Country'] == country]
    plt.plot(country_data['Year'], country_data['FertilityRate'], marker='o', label=country)

plt.title('Fertility Rate Over Time by Country')
plt.xlabel('Year')
plt.ylabel('Fertility Rate (Children per Woman)')
plt.legend(title='Country', bbox_to_anchor=(1.05, 1), loc='upper left')
plt.tight_layout()
plt.grid(True)
plt.show()