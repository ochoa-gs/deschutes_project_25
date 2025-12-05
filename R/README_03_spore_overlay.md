# 03 - Spore & Hydro Overlay Analysis

##  Purpose

This is the core analysis script. It merges the clean spore data (all_spores_daily_mean_clean.csv) with the clean environmental data (cleaned_combined_data.csv) to analyze the relationship between *C. shasta* spores, river discharge, and water temperature.

##  Dependencies

* **R Packages:** readr, dplyr, lubridate, ggplot2, scales, patchwork, here
* **Data Dependencies:** This script requires the successful completion of **Scripts 01 and 02**.

##  Key Join Operations

The script performs a **many-to-many join** (relationship = "many-to-many") to match every spore sampling event (OS and PT) to **both** MoodyMadras and Madras gauge readings on the same day. This creates the foundational combo data frame for comparative plots.

##  Key Analysis & Plotting

1.  **Time Series Overlay:** Daily spore concentrations, discharge, and temperature are plotted over time, utilizing dual axes and **facet_wrap(~ site_id)** to clearly separate MoodyMadras and Madras hydro data.
2.  **Flow-Spore Relationship:** Spore concentration (log-transformed) is plotted against discharge, categorized by season to identify flow-dependence.
3.  **Monthly Means:** Monthly averages are calculated and plotted to reveal seasonal trends in both environmental and spore data.

##  Outputs

* Plots in plots/spore_overlay_outputs/:
    * spores_flow_temp_overlay_all.png
    * timeseries_flow_temp_TOP__spores_BOTTOM.png
    * monthly_flowtemp_TOP__spores_BOTTOM.png
    * spores_vs_discharge_by_site.png