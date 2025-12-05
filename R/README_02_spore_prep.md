# 02 - Spore Data Cleaning & Daily Aggregation

## Purpose

This script processes the raw C. shasta spore concentration data from the Oak Springs (OS) and Pelton Trap (PT) sites. It calculates daily mean spore concentrations, handles zero values, and standardizes the output for merging with the environmental data.

## Dependencies

* **R Packages:** readr, dplyr, lubridate, janitor, here

##  Inputs

* data/spores_deschutes/raw/all_spores_raw.csv (Raw spore data, potentially containing multiple readings per day/site).

##  Key Processing Steps

1.  **Load and Standardize:** Raw spore concentration data is loaded.
2.  **Date Formatting:** Dates are standardized and a water_year column is added.
3.  **Daily Mean Calculation:** Daily mean spore concentrations (mean_spores_l) are calculated for each unique site (OS/PT) and date combination.
4.  **Zero Handling:** Zero spore counts are maintained, ensuring that $log_{10}(\text{spores} + 1)$ transformations can be performed later.

##  Outputs

* data/spores_deschutes/clean/all_spores_daily_mean_clean.csv (The final daily aggregated spore data file.)