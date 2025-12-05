# ================================================================
# Spore Count Data Cleaning and Preparation (OS & PT Sites)
# ================================================================

# ---- Libraries --------------------------------------------------
library(dplyr)
library(lubridate)
library(here)
library(readr) 
library(janitor) 

# ---- Path Definitions -------------------------------------------
spores_raw_dir <- here("data", "spores_deschutes", "raw")
spores_clean_dir <- here("data", "spores_deschutes", "clean")

os_raw_path <- here(spores_raw_dir, "raw_os_spores.csv")
pt_raw_path <- here(spores_raw_dir, "raw_pt_spores.csv")

# Ensure the output directory exists
if (!dir.exists(spores_clean_dir)) {
  dir.create(spores_clean_dir, recursive = TRUE)
}

##------Read raw CSVs and Clean Names ----
os_raw <- readr::read_csv(os_raw_path, show_col_types = FALSE) |> janitor::clean_names() 
pt_raw <- readr::read_csv(pt_raw_path, show_col_types = FALSE) |> janitor::clean_names()

##-------Cleaning function 
clean_spores <- function(df) {
  
  df %>%
    rename(
      date = date,
      site = site,
      rep = sample,
      spores_l = cs_spl 
    ) %>%
    mutate(
      date = as.Date(date, format = "%m/%d/%Y"),
      site = toupper(trimws(site)),
      rep = suppressWarnings(as.integer(rep)),
      spores_l = suppressWarnings(as.numeric(spores_l)),
      water_year = year(date) + if_else(month(date) >= 10, 1L, 0L)
    ) %>%
    filter(!is.na(date), !is.na(site), !is.na(rep)) %>%
    arrange(site, date, rep)
  
}

##------Apply to each file ----
os_spores_clean <- clean_spores(os_raw)
pt_spores_clean <- clean_spores(pt_raw)

##------Combine both sites ----
spores_clean <- bind_rows(os_spores_clean, pt_spores_clean)

##------OPTIONAL: collapse replicates to one value per date–site ----
spores_daily <- spores_clean %>%
  group_by(site, date, water_year) %>%
  summarise(
    n_reps = sum(!is.na(spores_l)),
    mean_spores_l = mean(spores_l, na.rm = TRUE), 
    .groups = "drop"
  )

##-----Save outputs (Using simplified path definitions) ----

#------Save OS clean
readr::write_csv(
  os_spores_clean,
  here(spores_clean_dir, "os_spores_clean.csv")
)

#------Save PT clean
readr::write_csv(
  pt_spores_clean,
  here(spores_clean_dir, "pt_spores_clean.csv")
)

#------Save All (replicates included)
readr::write_csv(
  spores_clean,
  here(spores_clean_dir, "all_spores_clean_replicates.csv") # Renamed for clarity
)

#------Save All (daily means)
readr::write_csv(
  spores_daily,
  here(spores_clean_dir, "all_spores_daily_mean_clean.csv")
)