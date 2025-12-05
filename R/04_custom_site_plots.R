# =======================================================================
# 04_custom_site_plots.R
# Generates highly formatted, single-site, single-water-year plots
# and log-transformed exploratory data analysis (EDA) plots.
# DEPENDENCIES: Requires data files created by Scripts 01 and 02.
# =======================================================================

# ------------------------------- 1. SETUP & DATA LOADING ---------------------------------
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(janitor)
  library(ggplot2)
  library(scales)
  library(patchwork)
  library(here)
  library(zoo) # Needed for rollmean
  library(rlang) # Needed for the .data[[y_var]] syntax
})

# Define the output directory for these specific plots
out_dir_04 <- here("plots", "custom_site_plots")
if (!dir.exists(out_dir_04)) dir.create(out_dir_04, recursive = TRUE)

# --- Load the gauge data (Output from Script 01) ---
gauge_path <- here("data", "watergauge_deschutes", "madras_watergage_clean", "cleaned_combined_data.csv")
gauge <- read_csv(gauge_path, show_col_types = FALSE) %>%
  mutate(date = as_date(date)) %>%
  # Rename 'temp_c' to 'madras_water_temp' for function compatibility
  rename(madras_water_temp = temp_c)

# --- Load the spore data (Output from Script 02) ---
spores_daily_path <- here("data", "spores_deschutes", "clean", "all_spores_daily_mean_clean.csv")
spores_daily <- readr::read_csv(spores_daily_path, show_col_types = FALSE) %>%
  mutate(
    date = as_date(date),
    site = toupper(site)
  )

# ------------------------------- 2. SINGLE SITE OVERLAY FUNCTION ---------------------------------

#' Plots discharge/temp and spores for a specific site and water year.
#' Includes 7-day rolling mean and custom styling ---Pick site code and WY here
plot_spores_flow_temp_site <- function(site_code = "PT", wy_select = 2019) {
  
  site_code <- toupper(site_code)
  
  # Gauge data for that WY (uses gauge loaded globally)
  gauge_wy <- gauge %>%
    filter(water_year == wy_select) %>%
    arrange(date) %>%
    mutate(
      q7_mean = zoo::rollmean(discharge_cms, 7, fill = NA, align = "right")
    )
  
  # Spore data (uses spores_daily loaded globally)
  spores_wy <- spores_daily %>%
    filter(water_year == wy_select, site == site_code) %>%
    arrange(date)
  
  if (nrow(spores_wy) == 0) {
    stop("No spore data found for site = ", site_code,
         " and water_year = ", wy_select)
  }
  
  # Temperature scaling for dual axis
  temp_scale <- max(gauge_wy$discharge_cms, na.rm = TRUE) /
    max(gauge_wy$madras_water_temp, na.rm = TRUE)
  #------------------temp ticks
  temp_ticks <- pretty(range(gauge_wy$madras_water_temp, na.rm = TRUE), n = 5)
  
  gauge_wy <- gauge_wy %>%
    mutate(temp_scaled = madras_water_temp * temp_scale)
  
  # Water year date range
  wy_start <- as.Date(paste0(wy_select - 1, "-10-01"))
  wy_end 	 <- as.Date(paste0(wy_select, "-09-30"))
  
  # Month breaks for the axis
  month_breaks <- seq(wy_start, wy_end, by = "1 month")
  
  # ---- TOP PANEL: FLOW + TEMP ----
  p_top <- ggplot(gauge_wy, aes(x = date)) +
    geom_line(aes(y = discharge_cms, color = "Daily discharge"),
              linewidth = 0.7, alpha = 0.75) +
    geom_line(aes(y = q7_mean, color = "7-day mean discharge"),
              linewidth = 0.9) +
    geom_line(aes(y = temp_scaled, color = "Temperature (°C)"),
              linewidth = 0.8, linetype = "dashed") +
    scale_x_date(
      breaks = month_breaks,
      labels = format(month_breaks, "%b"),
      expand = c(0.01, 0.01)
    ) +
    scale_y_continuous(
      name = expression("Discharge (m"^3*"/s)"),
      breaks = scales::pretty_breaks(6),
      sec.axis = sec_axis(
        ~ . / temp_scale,
        name 	= "Temperature (°C)",
        breaks = temp_ticks
      )
    ) +
    scale_color_manual(
      values = c(
        "Daily discharge" = "grey40",
        "7-day mean discharge" = "blue",
        "Temperature (°C)" = "red"
      ),
      name = NULL
    ) +
    labs(
      title = paste0(
        "Site ", site_code, " – Water Year ", wy_select,
        " (", wy_start, " to ", wy_end, ")"
      ),
      x = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.title.y.left 	= element_text(color = "blue"),
          axis.title.y.right = element_text(color = "red"),
          axis.text.y.right 	= element_text(color = "red"),
          axis.ticks.y.right = element_line(color = "red"),
          axis.line.y.right 	= element_line(color = "red"),
          axis.ticks.y.left = element_line(color = "grey20"),
          axis.line.y.left 	= element_line(color = "grey20"),
          legend.position 	 	= "bottom",
          panel.grid.minor 	 = element_blank()
    )
  
  
  # ---- BOTTOM PANEL: SPORES ----
  p_bottom <- ggplot(spores_wy, aes(x = date, y = mean_spores_l)) +
    geom_point(color = "darkgreen", size = 2, alpha = 0.9) +
    geom_line(color = "darkgreen", alpha = 0.6) +
    scale_x_date(
      breaks = month_breaks,
      labels = format(month_breaks, "%b"),
      expand = c(0.01, 0.01)
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(5)) +
    labs(
      title = "C. shasta spore concentration",
      x = "Month",
      y = "Spore concentration (spores/L)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )
  
  # Stack the panels
  p_combined <- p_top / p_bottom + patchwork::plot_layout(heights = c(2, 1))
  
  # Save the plot
  filename <- file.path(out_dir_04, paste0("site_", site_code, "_WY", wy_select, "_hydro_spores.png"))
  ggsave(filename, plot = p_combined, width = 12, height = 9, dpi = 300)
  message("Saved custom hydrograph: ", filename)
  
  return(p_combined)
}


# ------------------------------- 3. EXPLORATORY DATA ANALYSIS (EDA) PLOTS ---------------------------------

## --- Log spores by month all years (all sites) ---
plot_monthly_spore_all_sites <- function() {
  p <- ggplot(
    spores_daily,
    aes(
      x = month(date, label = TRUE),
      y = log10(mean_spores_l + 1)
    )
  ) +
    geom_boxplot(
      fill = "forestgreen",
      color = "black",
      alpha = 0.6,
      outlier.shape = 21,
      outlier.fill = "white",
      outlier.color = "black",
      width = 0.6
    ) +
    geom_jitter(
      aes(color = site),
      width = 0.15,
      alpha = 0.8,
      size = 2.2
    ) +
    scale_color_manual(
      values = c("OS" = "#1f78b4", 	# blue
                 "PT" = "#e31a1c"), 	# red/pink
      name = "Sampling site", 	 	# Legend title (your “index”)
      labels = c("OS = Oak Springs", "PT = Pelton Trap")
    ) +
    scale_y_continuous(
      breaks = pretty_breaks(6),
      expand = expansion(mult = c(0.05, 0.05))
    ) +
    labs(
      title = "Monthly C. shasta Spore Concentrations by Site (All Water Years)",
      x = "Month",
      y = "log10(spores/L + 1)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black"),
      axis.ticks.x = element_line(color = "black"),
      axis.ticks.y = element_line(color = "black"),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 15),
      legend.position = "right"
    )
  
  # Save the plot
  filename <- file.path(out_dir_04, "spores_monthly_boxplot_all_sites.png")
  ggsave(filename, plot = p, width = 10, height = 7, dpi = 300)
  message("Saved multi-site monthly boxplot: ", filename)
  
  return(p)
}


## --- Raw vs Log Box plot function --- - Pick Site code and WY here
plot_monthly_spore_boxplot <- function(site_code = "PT",
                                       wy_select = 2016,
                                       log_scale = TRUE) {
  
  site_code <- toupper(site_code)
  
  dat <- spores_daily %>%
    filter(site == site_code,
           water_year == wy_select) %>%
    mutate(
      month_lab 	= month(date, label = TRUE),
      log_spores = log10(mean_spores_l + 1)
    )
  
  if (nrow(dat) == 0) {
    stop("No spore data for site = ", site_code,
         " in water year ", wy_select)
  }
  
  if (log_scale) {
    y_var 	<- "log_spores"
    y_label <- "log10(spores/L + 1)"
    title_suffix <- " (log scale)"
  } else {
    y_var 	<- "mean_spores_l"
    y_label <- "Spore concentration (spores/L)"
    title_suffix <- " (raw scale)"
  }
  
  p <- ggplot(dat, aes(x = month_lab, y = .data[[y_var]])) +
    geom_boxplot(fill = "forestgreen", alpha = 0.6, color = "black") +
    scale_y_continuous(breaks = pretty_breaks(6)) +
    labs(
      title = paste0("Monthly C. shasta Spore Concentrations (",
                     site_code, ", WY ", wy_select, ")", title_suffix),
      x = "Month",
      y = y_label
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
  
  return(p)
}

### --- Side by side raw vs. log boxplots ---Pick site code and WY here
plot_raw_vs_log_boxplots <- function(site_code = "PT", wy_select = 2016) {
  p_raw <- plot_monthly_spore_boxplot(site_code, wy_select, log_scale = FALSE)
  p_log <- plot_monthly_spore_boxplot(site_code, wy_select, log_scale = TRUE)
  
  p_combined <- p_raw + p_log + plot_layout(ncol = 2)
  
  # Save the plot
  filename <- file.path(out_dir_04, paste0("site_", site_code, "_WY", wy_select, "_raw_vs_log_boxplot.png"))
  ggsave(filename, plot = p_combined, width = 10, height = 5, dpi = 300)
  message("Saved side-by-side boxplot: ", filename)
  
  return(p_combined)
}

### --- Histograms raw vs log transform ----Pick sitecode and WY here
plot_spore_histograms <- function(site_code = "PT", wy_select = 2016) {
  site_code <- toupper(site_code)
  
  dat <- spores_daily %>%
    filter(site == site_code,
           water_year == wy_select) %>%
    mutate(log_spores = log10(mean_spores_l + 1))
  
  if (nrow(dat) == 0) {
    stop("No spore data for site = ", site_code,
         " in water year ", wy_select)
  }
  
  p_raw <- ggplot(dat, aes(x = mean_spores_l)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
    labs(
      title = paste0("Raw spore concentrations (", site_code,
                     ", WY ", wy_select, ")"),
      x = "Spore concentration (spores/L)",
      y = "Count"
    ) +
    theme_minimal(base_size = 12)
  
  p_log <- ggplot(dat, aes(x = log_spores)) +
    geom_histogram(bins = 30, fill = "darkgreen", color = "black", alpha = 0.7) +
    labs(
      title = paste0("log10(spores/L + 1) (", site_code,
                     ", WY ", wy_select, ")"),
      x = "log10(spores/L + 1)",
      y = "Count"
    ) +
    theme_minimal(base_size = 12)
  
  p_combined <- p_raw + p_log + plot_layout(ncol = 2)
  
  # Save the plot
  filename <- file.path(out_dir_04, paste0("site_", site_code, "_WY", wy_select, "_raw_vs_log_hist.png"))
  ggsave(filename, plot = p_combined, width = 10, height = 5, dpi = 300)
  message("Saved side-by-side histogram: ", filename)
  
  return(p_combined)
}


# ------------------------------- 4. EXECUTION EXAMPLES ---------------------------------
# NOTE: Run these functions after defining them to generate your plots.

## --- Example 1: High-Quality Single Site Overlay Plot ---
print(plot_spores_flow_temp_site("PT", 2020))
print(plot_spores_flow_temp_site("OS", 2018)) # You can choose another year/site

## --- Example 2: Monthly Spores Across All Sites/Years (Boxplot) ---
print(plot_monthly_spore_all_sites())

## --- Example 3: Raw vs. Log Boxplots for EDA (Single WY) ---
print(plot_raw_vs_log_boxplots("OS", 2016))

## --- Example 4: Raw vs. Log Histograms for EDA (Single WY) ---
print(plot_spore_histograms("PT", 2016))