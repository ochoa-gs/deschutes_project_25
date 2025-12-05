# 04 - Custom Site-Specific & Exploratory Data Analysis (EDA) Plots

##  Purpose

This script serves two primary goals:
1.  Generate **high-resolution, publication-quality figures** focusing on a single spore site and a single water year.
2.  Provide **Exploratory Data Analysis (EDA) visualizations** (boxplots and histograms) essential for assessing the **log-transformation** of the highly skewed spore concentration data.

##  Dependencies

* **R Packages:** readr, dplyr, lubridate, ggplot2, scales, patchwork, here, **zoo** (for rolling means), **rlang** (for non-standard evaluation in functions).
* **Data Dependencies:** This script requires the successful completion of **Scripts 01 and 02** to load the global gauge and spores_daily data frames.

##  Key Functions & Outputs

The script is primarily built around reusable R functions for targeted plotting:

### 1. High-Quality Overlay (plot_spores_flow_temp_site)

This function generates a two-panel, stacked time-series plot for a chosen site (e.g., "PT") and water year (e.g., 2020).
* **Top Panel:** Daily discharge, **7-day rolling mean discharge**, and temperature (dual axis).
* **Bottom Panel:** Raw spore concentrations.
* **Output Example:** plots/custom_site_plots/site_PT_WY2020_hydro_spores.png

### 2. Spore Distribution Analysis (EDA)

These functions are used to validate data transformations and visualize seasonal distribution.

| Function | Plot Type | Purpose |
| :--- | :--- | :--- |
| plot_monthly_spore_all_sites | Boxplot (All Years) | Shows **log-transformed** spore distribution by month across **both sites** and all water years. |
| plot_raw_vs_log_boxplots | Side-by-Side Boxplots | Compares the monthly distribution of **raw vs. log-transformed** spores for a specific site/WY. |
| plot_spore_histograms | Side-by-Side Histograms | Visually confirms that the $log_{10}(\text{spores} + 1)$ transformation improves normality for statistical modeling. |

##  Outputs

* Plots in plots/custom_site_plots/: Various high-resolution, site-specific hydrographs, boxplots, and histograms based on the function calls executed at the end of the script.