# Data Source
UNHCR has created the refugees R package designed to facilitate access to the data within the Refugee Data Finder. It provides an easy-to-use interface to the datasets, which cover forcibly displaced populations, including refugees, asylum-seekers and internally displaced people, stateless people, and others over a span of more than 70 years.

This package provides data from three major sources:
1. Data from UNHCR's annual statistical activities dating back to 1951.
2. Data from the United Nations Relief and Works Agency for Palestine Refugees in the Near East (UNRWA), specifically for registered Palestine refugees under UNRWA's mandate.
3. Data from the Internal Displacement Monitoring Centre (IDMC) on people displaced within their country due to conflict or violence.

The data within the refugees package is updated at the same time as the Refugee Data Finder, twice per year.

# Installing Package
```
install.packages("refugees", repos = "http://cran.us.r-project.org")

library(refugees)
```