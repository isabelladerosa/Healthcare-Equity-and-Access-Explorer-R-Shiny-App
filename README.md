# Overview

The Healthcare Access and Equity Explorer is an interactive R Shiny application designed to support explanatory analysis of healthcare access and equity across US counties. The app allows users to look at how healthcare resources like physicians, dentists, hospitals, and hospital beds vary geographically and how these patterns relate to socioeconomic factors like poverty, income, an insurance coverage.

This project emphasizes county-level which shows more inconsistencies that are often overlooked when the data is analyzed at the state or national level.

The app is intended for exploratory data analysis and is suitable for students, policymakers,researchers, and others interested in looking into structural inequalities in healthcare access.

# Data Sources

Source: https://data.hrsa.gov/data/download Variables: - Population - Poverty Percent - Median Family Income - Uninsured Rates - Active Physicians - Primary Care Physician Rates - Dentists in Private Practices - Hospitals and Hospital Beds

# App Features

### Introduction Tab

This provides some background on healthcare access and equity. It also provides the purpose of the app and its data sources and context.

### Univariate Analysis Tab

This allows the user to select a single healthcare or socioeconomic variable and view the distributions using histograms. There is an option to apply log scaling.

### Bivariate Analysis Tab

This compares two variables using scatter plots with an optional linear trend line and optional log scaling on either axis. This is designed to explore relationships between two variables.

### Data Table Tab

Here you can view the cleaned dataset.
