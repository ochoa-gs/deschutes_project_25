# =======================================================================
# 03_spore_overlay_analysis.R
# Overlay cleaned C. shasta spore data (OS/PT) with gauge discharge + temperature
# =======================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(janitor)
  library(ggplot2)
  library(scales)
  library(patchwork)
  library(here)
  library(rlang) # Used for standardizing messages (though not strictly necessary here)
})

# ------------------------------- Paths ---------------------------------

# New path 
gauge_path <- here(
  "data", "watergauge_deschutes", "madras_watergage_clean", "cleaned_combined_data.csv" # UPDATED NAME
)

# New path for the daily mean spore data (OUTPUT of Script 02)
spores_daily_path <- here(
  "data", "spores_deschutes", "clean", "all_spores_daily_mean_clean.csv" # UPDATED PATH
)

# ------------------------------- Output folder -------------------------------
out_dir <- here("plots", "spore_overlay_outputs")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# --------------------------- Load gauge data ---------------------------
# : site_id, date, water_year, discharge_cms, temp_c (all cleaned by Script 01)
gauge <- read_csv(gauge_path, show_col_types = FALSE) |>
  # janitor::clean_names() is NOT needed if Script 01 uses write_csv
  mutate(date = as_date(date))

# Rename 'temp_c' to the old name 'madras_water_temp' for compatibility with old plotting code
gauge <- gauge |>
  rename(madras_water_temp = temp_c)

# -------------------- Monthly Gauge Means --------------------------
gauge_month <- gauge |>
  # NOTE: We still group by site_id to ensure monthly averages are site-specific
  mutate(month = floor_date(date, "month")) |>
  group_by(site_id, month) |>
  summarise(
    discharge_cms_m = mean(discharge_cms, na.rm = TRUE),
    madras_water_temp_m = mean(madras_water_temp, na.rm = TRUE),
    .groups = "drop"
  )
# ------------------------- Load spore data -----------------------------

# EXPECTING: site, date, water_year, mean_spores_l (all cleaned by Script 02)
spores_daily <- readr::read_csv(spores_daily_path, show_col_types = FALSE) |>
  # janitor::clean_names() is NOT needed if Script 02 uses write_csv
  mutate(
    date = as_date(date),
    site = toupper(site)
  )

# ---------------------- Combine Data ----------------------
# NOTE: gauge data has 'site_id', spore data has 'site'. Need to align site names for join.
combo <- spores_daily |>
  # === FIX A: ADD relationship = "many-to-many" to silence Warning 1 ===
  left_join(gauge, by = c("date", "water_year"), relationship = "many-to-many") |>
  # === FIX C: Filter out NA spore data to resolve "Removed rows" warnings (2, 3, etc.) ===
  filter(!is.na(mean_spores_l)) |>
  # Filter out NA gauge data (Existing check)
  filter(!is.na(discharge_cms), !is.na(madras_water_temp)) |>
  mutate(log10_spores_l = log10(mean_spores_l + 1)) |>
  # ADD SEASONALITY:
  mutate(
    season = case_when(
      month(date) %in% c(12, 1, 2) ~ "Winter",
      month(date) %in% c(3, 4, 5) ~ "Spring",
      month(date) %in% c(6, 7, 8) ~ "Summer",
      TRUE ~ "Fall"
    )
  )


# ----------------------------- Plot: ALL DATES -------------------------
# ... (Plotting code remains the same, using 'madras_water_temp')
temp_scale <- max(combo$discharge_cms, na.rm = TRUE) /
  max(combo$madras_water_temp, na.rm = TRUE)

combo_all <- combo |>
  mutate(temp_scaled = madras_water_temp * temp_scale)

p_all <- ggplot(combo_all, aes(x = date)) +
  geom_line(aes(y = discharge_cms, color = "Discharge (m³/s)"), linewidth = 0.6) +
  geom_line(aes(y = temp_scaled, color = "Temperature (°C)"),
            linewidth = 0.6, linetype = "dashed") +
  geom_point(aes(y = mean_spores_l, shape = site, color = "Spore concentration"),
             size = 2, alpha = 0.9) +
  scale_y_continuous(
    name = expression("Discharge (m"^3*"/s)"),
    sec.axis = sec_axis(~ . / temp_scale, name = "Temperature (°C)")
  ) +
  scale_color_manual(
    values = c("Discharge (m³/s)" = "blue",
               "Temperature (°C)" = "red",
               "Spore concentration" = "darkgreen"),
    breaks = c("Discharge (m³/s)", "Temperature (°C)", "Spore concentration"),
    name = NULL
  ) +
  scale_shape_manual(values = c(OS = 16, PT = 17), name = "Spore site") +
  labs(
    title = "Discharge, Temperature, and C. shasta Spores (OS/PT)",
    subtitle = "All available dates; secondary axis for temperature",
    x = "Date"
  ) +
  # ===ADD FACETING ===
  facet_wrap(~ site_id, ncol = 1) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title.y.right = element_text(color = "red"),
    axis.title.y.left = element_text(color = "blue")
  )

print(p_all)

ggsave(file.path(out_dir, "spores_flow_temp_overlay_all.png"),
       plot = p_all, width = 12, height = 7, dpi = 300)





# ------------------ Plot: SINGLE WATER YEAR (stacked) ------------------

wy_select <- 2016 # change this to whichever WY you want
log_spores <- TRUE # set to FALSE for raw spores/L

gauge_wy <- gauge |>
  filter(water_year == wy_select)

combo_wy <- spores_daily |>
  filter(water_year == wy_select) |>
  # === FIX B: ADD relationship = "many-to-many" to silence Warning 4 ===
  inner_join(gauge_wy, by = c("date", "water_year"), relationship = "many-to-many") |>
  # The previous filter on 'combo' already handles the spore NA, 
  # but we repeat it here just for the single WY plot for robustness:
  filter(!is.na(mean_spores_l)) |>
  filter(!is.na(discharge_cms), !is.na(madras_water_temp))

temp_scale_wy <- max(combo_wy$discharge_cms, na.rm = TRUE) /
  max(combo_wy$madras_water_temp, na.rm = TRUE)

combo_wy <- combo_wy |>
  mutate(
    temp_scaled = madras_water_temp * temp_scale_wy,
    spores_plot = if (log_spores) log10(mean_spores_l + 1) else mean_spores_l,
    spores_ylabel = if (log_spores)
      "Spore concentration (log10(spores/L + 1))"
    else
      "Spore concentration (spores/L)"
  )

wy_start <- as.Date(paste0(wy_select - 1, "-10-01"))
wy_end <- as.Date(paste0(wy_select,"-09-30"))

# Top: discharge + temp
p_top <- ggplot(combo_wy, aes(x = date)) +
  geom_line(aes(y = discharge_cms, color = "Discharge (m³/s)"), linewidth = 0.7) +
  geom_line(aes(y = temp_scaled, color = "Temperature (°C)"),
            linewidth = 0.7, linetype = "dashed") +
  scale_y_continuous(
    name = expression("Discharge (m"^3*"/s)"),
    sec.axis = sec_axis(~ . / temp_scale_wy, name = "Temperature (°C)")
  ) +
  scale_color_manual(
    values = c("Discharge (m³/s)" = "blue", "Temperature (°C)" = "red"),
    name = NULL
  ) +
  labs(
    title = paste0("Water Year ", wy_select, " (", wy_start, " to ", wy_end, ")"),
    subtitle = "Discharge (blue) and Temperature (red, dashed)"
  ) +
  # === ADD FACETING ===
  facet_wrap(~ site_id, ncol = 1) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red")
  )

# Bottom: spores by site
p_bottom <- ggplot(combo_wy, aes(x = date, y = spores_plot, color = site)) +
  geom_point(size = 1.8, alpha = 0.9) +
  geom_line(alpha = 0.6) +
  scale_color_manual(values = c(OS = "darkgreen", PT = "orange")) +
  labs(
    title = "C. shasta spores by site (OS, PT)",
    x = "Date",
    y = unique(combo_wy$spores_ylabel),
    color = "Site"
  ) +
  theme_minimal()

p_wy <- p_top / p_bottom + plot_layout(heights = c(2, 1))

print(p_wy)

# safer if/else for outfile
if (log_spores) {
  outfile <- file.path(out_dir, paste0("WY", wy_select, "_flow_temp__spores_log10.png"))
} else {
  outfile <- file.path(out_dir, paste0("WY", wy_select, "_flow_temp__spores.png"))
}

ggsave(outfile, plot = p_wy, width = 12, height = 9, dpi = 300)
message("Saved: ", outfile)

# ------------------- Relationship: spores vs discharge -----------------
# ... (Plotting code remains the same)
p_rel <- ggplot(combo, aes(x = discharge_cms, y = log10_spores_l)) +
  geom_point(aes(color = season), size = 2, alpha = 0.85) + # <--- NEW COLOR
  geom_smooth(method = "glm", formula = y ~ poly(x, 2), se = FALSE, color = "black") +
  # Remove scale_color_viridis_c and add a discrete scale:
  scale_color_manual(
    values = c("Winter" = "blue", "Spring" = "darkgreen",
               "Summer" = "red", "Fall" = "orange"),
    name = "Season"
  ) +
  facet_wrap(~ site) +
  labs(
    title = "Spore Concentration vs Discharge (by Site)",
    x = "Discharge (m³/s)",
    y = "Spore concentration (log10(spores/L +1))"
  ) +
  theme_minimal()

print(p_rel)

ggsave(file.path(out_dir, "spores_vs_discharge_by_site.png"),
       plot = p_rel, width = 10, height = 6, dpi = 300)

# ------------------- Time series: flow/temp + spores ------------------
# ... (Plotting code remains the same)
temp_scale_ts <- max(combo$discharge_cms, na.rm = TRUE) /
  max(combo$madras_water_temp, na.rm = TRUE)

combo_all_ts <- combo |>
  mutate(temp_scaled = madras_water_temp * temp_scale_ts)

p_flowtemp <- ggplot(combo_all_ts, aes(x = date)) +
  geom_line(aes(y = discharge_cms, color = "Discharge (m³/s)"), linewidth = 0.6) +
  geom_line(aes(y = temp_scaled, color = "Temperature (°C)"),
            linewidth = 0.6, linetype = "dashed") +
  facet_wrap(~ site, ncol = 1, scales = "free_x") +
  scale_y_continuous(
    name = expression("Discharge (m"^3*"/s)"),
    sec.axis = sec_axis(~ . / temp_scale_ts, name = "Temperature (°C)")
  ) +
  scale_color_manual(
    values = c("Discharge (m³/s)" = "blue", "Temperature (°C)" = "red"),
    name = NULL
  ) +
  labs(
    title = "Discharge (blue) and Temperature (red, dashed) by Site",
    x = NULL
  ) +
  facet_wrap(~ site, ncol = 1) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title.y.right = element_text(color = "red"),
    axis.title.y.left = element_text(color = "blue"),
    strip.text = element_text(face = "bold")
  )

p_spores_ts <- ggplot(combo_all_ts, aes(x = date, y = mean_spores_l, color = site)) +
  geom_point(size = 1.8, alpha = 0.9) +
  geom_line(alpha = 0.5) +
  facet_wrap(~ site, ncol = 1, scales = "free_x") +
  labs(
    title = "Spore Concentration (spores/L)",
    x = "Date",
    y = "spores/L",
    color = "Site"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

p_ts <- p_flowtemp / p_spores_ts + plot_layout(heights = c(2, 1)) +
  plot_annotation(title = "Hydrology & Spores Over Time (by Site)")

print(p_ts)

ggsave(file.path(out_dir, "timeseries_flow_temp_TOP__spores_BOTTOM.png"),
       plot = p_ts, width = 12, height = 9, dpi = 300)

# --------------------- Monthly means (smoother) -----------------------
# ... (The join here was fixed in the previous exchange)
spores_monthly <- combo |>
  mutate(month = floor_date(date, "month")) |>
  group_by(site, month) |>
  summarise(
    spores_l = mean(mean_spores_l, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # Now join with the standardized gauge means
  left_join(gauge_month, by = "month", relationship = "many-to-many")

# The new data frame for monthly plots is 'spores_monthly'
# Use the standardized columns for plotting (e.g., discharge_cms_m)
temp_scale_m <- max(spores_monthly$discharge_cms_m, na.rm = TRUE) /
  max(spores_monthly$madras_water_temp_m, na.rm = TRUE)

p_flowtemp_m <- ggplot(spores_monthly, aes(x = month)) +
  geom_line(aes(y = discharge_cms_m, color = "Discharge (m³/s)")) +
  geom_line(aes(y = madras_water_temp_m * temp_scale_m, color = "Temperature (°C)"),
            linetype = "dashed") +
  
  facet_wrap(~ site, ncol = 1) +
  scale_y_continuous(
    name = expression("Discharge (m"^3*"/s)"),
    sec.axis = sec_axis(~ . / temp_scale_m, name = "Temperature (°C)")
  ) +
  scale_color_manual(
    values = c("Discharge (m³/s)" = "blue", "Temperature (°C)" = "red"),
    name = NULL
  ) +
  labs(
    title = "Monthly Mean Discharge (blue) & Temperature (red, dashed)",
    x = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

p_spores_m <- ggplot(spores_monthly, aes(x = month, y = spores_l, color = site)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ site, ncol = 1) +
  labs(
    title = "Monthly Mean Spore Concentration",
    x = "Month",
    y = "spores/L",
    color = "Site"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

p_month_stack <- p_flowtemp_m / p_spores_m + plot_layout(heights = c(2, 1)) +
  plot_annotation(title = "Monthly Means: Hydrology & Spores (by Site)")

print(p_month_stack)

ggsave(file.path(out_dir, "monthly_flowtemp_TOP__spores_BOTTOM.png"),
       plot = p_month_stack, width = 12, height = 9, dpi = 300)

message("✅ Spore overlay + relationship + monthly plots saved to: ", out_dir)