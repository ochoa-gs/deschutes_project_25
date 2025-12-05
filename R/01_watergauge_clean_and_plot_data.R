# ================================================================
# Deschutes Gauge Pipeline — Water Years (Oct 1 → Sep 30)
# Multi-Site Data Cleaning, Combining, and Analysis
# ================================================================

# ---- Libraries --------------------------------------------------
library(readr)
library(dplyr)
library(lubridate)
library(janitor)
library(ggplot2)
library(zoo)
library(scales)
library(here)
library(rlang) # Needed for dynamic column naming in the function

# ---- CSV paths --------------------------------------------------

# Path for original Moody-Madras averaged data
moody_madras_raw_path <- here(
  "data",
  "watergauge_deschutes",
  "madras_watergage_raw",
  "raw_moody_madras_USGS_flow_n_temp.csv"
)

# Path for the new Madras dataset
madras_raw_path <- here(
  "data",
  "watergauge_deschutes",
  "madras_watergage_raw",
  "raw_madras_USGS_flow_n_temp.csv"
)

# Path for the combined cleaned data (OUTPUT)
combined_clean_path <- here(
  "data",
  "watergauge_deschutes",
  "madras_watergage_clean",
  "cleaned_combined_data.csv"
)

# ---- Data Cleaning Function -------------------------------------

#' Cleans, standardizes, and calculates water-year information for a single site.
#'
#' @param file_path Path to the raw CSV file.
#' @param site_name Unique ID for the site (e.g., "Madras_USGS").
#' @param discharge_col Name of the discharge column in the raw CSV.
#' @param temp_col Name of the temperature column in the raw CSV.
clean_and_prep_data <- function(file_path, site_name, discharge_col, temp_col) {
  
  # Use rlang::sym to handle column names dynamically within dplyr verbs
  discharge_col_sym <- rlang::sym(discharge_col)
  temp_col_sym <- rlang::sym(temp_col)
  
  df <- readr::read_csv(
    file_path,
    na = c("", "NA", "NaN", "-9999"),
    show_col_types = FALSE
  ) |>
    janitor::clean_names() |> # Converts headers to snake_case
    mutate(
      # Standardize column names based on function arguments
      date = mdy(date),
      
      # Select the correct raw columns using !! and rename them
      discharge_cfs = !!discharge_col_sym,
      temp_c = !!temp_col_sym,
      
      # Convert discharge from CFS to CMS and add Site ID
      discharge_cms = discharge_cfs * 0.0283168,
      site_id = site_name, # This line correctly assigns the site name
      
      # Add water-year info (Water Year starts Oct 1, ends Sep 30)
      month_num = month(date),
      water_year = if_else(month_num >= 10, year(date) + 1L, year(date)),
      month_lab = factor(
        month(date, label = TRUE, abbr = TRUE),
        levels = c("Oct","Nov","Dec","Jan","Feb","Mar",
                   "Apr","May","Jun","Jul","Aug","Sep")
      )
    ) |>
    # Select standardized columns for combining
    select(
      site_id, date, water_year, month_num, month_lab,
      discharge_cms, temp_c
    )
  
  return(df)
}

# ---- Read, Clean, Transform (Separate Datasets) ------------------

# 1. Processing the original (Moody-Madras) dataset
df_moody <- clean_and_prep_data(
  file_path = moody_madras_raw_path,
  
  site_name = "MoodyMadras", 
  # NOTE: Headers were converted to snake_case by janitor::clean_names()
  discharge_col = "moodymadras_avg_discharge",
  temp_col = "moody_madras_avg_water_temp"
)

# 2. Process the new (Madras) dataset
df_madras <- clean_and_prep_data(
  file_path = madras_raw_path,
  # --- CHANGE HERE: Using a simpler, clearer site_name ---
  site_name = "Madras",
  # NOTE: Headers were converted to snake_case by janitor::clean_names()
  discharge_col = "madras_river_discharge_cfs",
  temp_col = "madras_water_temp"
)

# 3. Combine the datasets
df_combined <- bind_rows(df_moody, df_madras)
# ... (rest of the script is unchanged and is correct) ...
# 3. Combine the datasets
df_combined <- bind_rows(df_moody, df_madras)

# -----------------------------------------------------------------

# ---- CREATE MONTHLY SUMMARY (Combined Data) ---------------------
# NOTE: Grouping includes site_id for separate monthly calculations
df_monthly_combined <- df_combined |>
  group_by(site_id, water_year, month_num, month_lab) |>
  summarise(
    discharge_cms = mean(discharge_cms, na.rm = TRUE),
    temp_c = mean(temp_c, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Create output folders --------------------------------------
if (!dir.exists(here("docs"))) dir.create(here("docs"), recursive = TRUE)
if (!dir.exists(here("plots"))) dir.create(here("plots"), recursive = TRUE)
#------Ensure the output data folder exists
if (!dir.exists(here("data", "watergauge_deschutes", "madras_watergage_clean"))) {
  dir.create(here("data", "watergauge_deschutes", "madras_watergage_clean"), recursive = TRUE)
}
# ---- Save cleaned combined daily dataset ------------------------
readr::write_csv(
  df_combined,
  combined_clean_path
)

message("Saved combined daily data: ", combined_clean_path)

# ================================================================
# PLOTS (All plots now use df_combined or df_monthly_combined)
# ================================================================

# ---- 1) Daily discharge by water year ---------------------------
# Plotting combined data, faceted by site_id and water_year
p_daily <- ggplot(df_combined, aes(x = date, y = discharge_cms, color = site_id)) +
  geom_line() +
  facet_grid(site_id ~ water_year, scales = "free_x") + # Use facet_grid for clarity
  scale_x_date(
    breaks = function(x) {
      wy_start <- as.Date(paste0(lubridate::year(min(x)), "-10-01"))
      seq(
        from = wy_start,
        to = wy_start + lubridate::period(11, "months"),
        by = "3 months"
      )
    },
    date_labels = "%b",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    title = "Daily Discharge by Site and Water Year (Oct 1 → Sep 30)",
    x = "Month (Water Year)",
    y = expression("Discharge (m"^3*"/s)"),
    color = "Site"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(p_daily)

if (interactive()) {
  ggsave(
    filename = here("plots", "combined_hydrograph_daily_by_wy.png"),
    plot = p_daily,
    width = 14, # Increased width for new facet grid
    height = 8,
    dpi = 300
  )
}

# ---- 2) 7-day rolling mean discharge ----------------------------

df_roll_combined <- df_combined |>
  arrange(date) |>
  group_by(site_id, water_year) |> # Group by site_id too
  mutate(q7_mean = zoo::rollmean(discharge_cms, 7, fill = NA, align = "right")) |>
  ungroup()

p_roll <- ggplot(df_roll_combined, aes(x = date)) +
  geom_line(aes(y = discharge_cms, color = site_id), alpha = 0.4) +
  geom_line(aes(y = q7_mean, color = site_id), size = 1) +
  facet_grid(site_id ~ water_year, scales = "free_x") +
  scale_x_date(
    breaks = function(x) {
      wy_start <- as.Date(paste0(lubridate::year(min(x)), "-10-01"))
      seq(
        from = wy_start,
        to = wy_start + lubridate::period(11, "months"),
        by = "3 months"
      )
    },
    date_labels = "%b",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    title = "Daily (light) vs 7-Day Mean (dark) Discharge by Site and Water Year",
    x = "Month (Water Year)",
    y = expression("Discharge (m"^3*"/s)"),
    color = "Site"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(p_roll)

if (interactive()) {
  ggsave(
    filename = here("plots", "combined_hydrograph_7day_by_wy.png"),
    plot = p_roll,
    width = 14,
    height = 8,
    dpi = 300
  )
}

# ---- 3) Monthly mean discharge + temperature --------------------
# NOTE: Uses df_monthly_combined
p_month <- ggplot(df_monthly_combined, aes(x = month_lab, y = discharge_cms, group = 1, color = site_id)) +
  geom_line() +
  geom_point(size = 1) +
  facet_grid(site_id ~ water_year, drop = TRUE, scales = "free_y") +
  scale_x_discrete(drop = FALSE) +
  
  scale_color_manual(
    values = c("MoodyMadras" = "blue", "Madras" = "darkblue"),
    name = "Site"
  ) +
  labs(
    title = "Monthly Mean Discharge by Site and Water Year (Oct → Sep)",
    x = "Month (water year order)",
    y = expression("Discharge (m"^3*"/s)"),
    color = "Site"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(size = 7),
    legend.position = "bottom"
  )

print(p_month)

if (interactive()) {
  ggsave(
    filename = here("plots", "combined_monthly_means_by_wy.png"),
    plot = p_month,
    width = 14,
    height = 8,
    dpi = 300
  )
}

# ---- 4) Monthly discharge + temperature overlay -----------------
# NOTE: This scale factor needs to be calculated per site for accurate overlay,
# but for simplicity, we calculate a single factor across all data.

scale_factor <- max(df_monthly_combined$discharge_cms, na.rm = TRUE) /
  max(df_monthly_combined$temp_c, na.rm = TRUE)

df_monthly_combined <- df_monthly_combined |>
  mutate(temp_scaled = temp_c * scale_factor)

p_month_overlay <- ggplot(df_monthly_combined, aes(x = month_lab, group = site_id)) +
  # Discharge Line
  geom_line(aes(y = discharge_cms, color = site_id)) +
  geom_point(aes(y = discharge_cms, color = site_id), size = 1) +
  # Temperature Line (using scaled temp)
  geom_line(aes(y = temp_scaled, linetype = "Temperature (°C)"), color = "red") +
  geom_point(aes(y = temp_scaled, shape = "Temperature (°C)"), color = "red", size = 1) +
  
  facet_grid(site_id ~ water_year, drop = TRUE, scales = "free_y") +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(
    name = expression("Discharge (m"^3*"/s)"),
    sec.axis = sec_axis(~ . / scale_factor, name = "Temperature (°C)")
  ) +
  scale_color_manual(
    values = c("MoodyMadras" = "blue", "Madras" = "darkblue"),
    name = "Discharge Site"
  ) +
  scale_linetype_manual(values = c("Temperature (°C)" = "dashed"), name = NULL) +
  scale_shape_manual(values = c("Temperature (°C)" = 16), name = NULL) +
  labs(
    title = "Monthly Mean Discharge & Temperature by Site and Water Year (Oct → Sep)",
    x = "Month"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(size = 7),
    legend.position = "bottom",
    axis.title.y.right = element_text(color = "red"),
    axis.title.y.left = element_text(color = "black")
  )

print(p_month_overlay)

if (interactive()) {
  ggsave(
    filename = here("plots", "combined_monthly_discharge_temp_overlay_by_wy.png"),
    plot = p_month_overlay,
    width = 14,
    height = 8,
    dpi = 300
  )
}

# ---- 5) Flow-duration curve -------------------------------------

fd_combined <- df_combined |>
  filter(!is.na(discharge_cms)) |>
  group_by(site_id) |> # Calculate FDC per site
  arrange(desc(discharge_cms)) |>
  mutate(
    rank = dplyr::row_number(),
    exceedance_pct = 100 * (rank / dplyr::n())
  ) |>
  ungroup()

p_fdc <- ggplot(fd_combined, aes(x = exceedance_pct, y = discharge_cms, color = site_id)) +
  geom_line(size = 1.2) +
  labs(
    title = "Flow-Duration Curve (All Available Days, by Site)",
    x = "Exceedance Probability (%)",
    y = expression("Discharge (m"^3*"/s)"),
    color = "Site"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_fdc)

if (interactive()) {
  ggsave(
    filename = here("plots", "combined_flow_duration_curve_all.png"),
    plot = p_fdc,
    width = 10,
    height = 6,
    dpi = 300
  )
}

# ---- 6) QA summary ----------------------------------------------
# NOTE: Grouping by site_id for QA
qa_missing_combined <- df_combined |>
  group_by(site_id) |>
  summarise(
    n_na_date = sum(is.na(date)),
    n_na_q = sum(is.na(discharge_cms)),
    n_na_temp = sum(is.na(temp_c)),
    .groups = "drop"
  )

print(qa_missing_combined)

readr::write_csv(
  qa_missing_combined,
  here("docs", "qa_missing_counts_combined.csv")
)

message("Saved QA summary: ", here("docs", "qa_missing_counts_combined.csv")) 