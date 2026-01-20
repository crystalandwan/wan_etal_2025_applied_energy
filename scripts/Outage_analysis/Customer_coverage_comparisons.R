# *****************************************************************************************
# Title: EAGLE-I Coverage and IEEE Customer Data Comparison
# Author: Heng Wan
# Date: 2025-10-30
# Purpose: This script processes EAGLE-I outage coverage data and compares it
#          with IEEE customer counts at the NERC subregion level. It performs
#          downscaling of state-level EAGLE-I data to county level based on
#          population, then aggregates to NERC subregions. Finally, it joins
#          this with NERC-level IEEE customer data and visualizes the comparison.
# Description:
#   1. Loads state-level EAGLE-I coverage data and county-level population data.
#   2. Downscales state-level EAGLE-I metrics (total customers, min/max covered)
#      to county level using population proportions.
#   3. Assigns NERC subregions to counties.
#   4. Aggregates county-level EAGLE-I metrics to NERC subregion level.
#   5. Loads IEEE customer data, aggregates it to NERC subregion level.
#   6. Joins the processed EAGLE-I and IEEE coverage data.
#   7. Prints national-level sums for cross-validation.
# Requirements:
#   - R packages: `data.table`, `dplyr`, `stringr`, `sf`, `lubridate`, `here`.
#   - Data files specified in `config.R`:
#     - EAGLE-I coverage history (`eaglei_coverage_path`)
#     - County 2020 population data (`county_pop_data_path`)
#     - Counties to NERC subregion shapefile  (`counties_nerc_shp_path`)
#     - IEEE customer counts (`ieee_customers_path`)
#   - A `config.R` file located in `scripts/Outage_analysis/` relative to `here::here()`.
# ******************************************************************************************

# Load Required Libraries ----
library(data.table) 
library(dplyr)      
library(stringr)    
library(sf)         
library(lubridate)
library(here)       

# Source Configuration File ----
config_path <- here::here("scripts", "Outage_analysis", "config.R")
if (!file.exists(config_path)) {
  stop("The configuration file does not exist. Ensure the correct path:", config_path)
}
source(config_path)

# --- Start of Main Script Logic ---

# 1. Load and Prepare EAGLE-I Coverage Data ----
message("Loading and preparing EAGLE-I coverage data...")
coverage <- fread(eaglei_coverage_path)

# Convert year from character to Date and extract year number
coverage[, year := as.Date(year, format = "%m/%d/%y")]
coverage$year <- year(coverage$year)

# Only keep CONUS data (excluding non-CONUS US states/territories)
coverage <- coverage[!coverage$state %in% c("AK", "AS", "GU", "HI", "MP", "PR", "VI"), ]

# 2. Load and Prepare County-level Population Data ----
message("Loading and preparing county-level population data...")
pop_data <- fread(county_pop_data_path)

# Select and rename relevant columns
pop_data <- pop_data[, c("GEOCODE", "STUSAB", "COUNTY", "U7H001")]
colnames(pop_data) <- c("GEOID", "state", "county", "pop2020")

# Pad GEOID to ensure 5-digit FIPS code consistency
pop_data$GEOID <- str_pad(pop_data$GEOID, width = 5, side = "left", pad = "0")

# Exclude non-CONUS US states/territories from population data
pop_data <- pop_data[!pop_data$state %in% c("HI", "AK", "PR")]

# 3. Downscale State-level EAGLE-I Coverage to County Level ----
message("Downscaling state-level EAGLE-I coverage to county level...")
# Initialize an empty dataframe to store results
county_results <- data.frame()

# Loop through each year (2018 to 2022)
for (yr in 2018:2022) {
  # Filter coverage data for the current year
  coverage_year <- coverage[coverage$year == yr, ]
  
  # Merge coverage data with population data
  merged_data <- merge(pop_data, coverage_year, by = "state", all.x = TRUE)
  
  # Calculate state-level population totals
  state_pop <- merged_data %>%
    group_by(state) %>%
    summarise(state_total_pop = sum(pop2020, na.rm = TRUE))
  
  # Join state population totals back to merged data
  merged_data <- merge(merged_data, state_pop, by = "state")
  
  # Calculate population proportion for each county
  merged_data$pop_proportion <- merged_data$pop2020 / merged_data$state_total_pop
  
  # Downscale the metrics based on population proportion
  merged_data <- merged_data %>%
    mutate(
      county_total_customers = total_customers * pop_proportion,
      county_min_covered = min_covered * pop_proportion,
      county_max_covered = max_covered * pop_proportion
    )
  
  # Select relevant columns for output
  year_results <- merged_data %>%
    select(year, state, county, GEOID, pop2020, 
           county_total_customers, county_min_covered, county_max_covered)
  
  # Append to results
  county_results <- bind_rows(county_results, year_results)
}

# 4. Map Counties to NERC Subregions ----
message("Mapping counties to NERC subregions...")
# Read in shapefile which maps each county to each NERC subregion
counties_NERCs_sub <- st_read(counties_nerc_shp_path)

# Categorize counties into NERC regions based on their ID
# This mapping needs to be consistent with the NERC region definitions.
counties_NERCs_sub <- counties_NERCs_sub %>%
  mutate(
    NERC = case_when(
      ID %in% c("1", "2", "6", "7") ~ "WECC",
      ID == "18" ~ "MRO",
      ID == "8" ~ "SPP",
      ID == "3" ~ "TRE",
      ID == "4" ~ "FRCC",
      ID == "17" ~ "RFC",
      ID %in% c("9", "10", "11", "12", "20") ~ "SERC",
      ID %in% c("5", "15") ~ "NPCC",
      TRUE ~ NA_character_ # Handle any IDs not explicitly mapped
    )
  )

# Drop geometry
counties_NERCs_sub <- st_drop_geometry(counties_NERCs_sub)

# Select only necessary columns
counties_NERCs_sub <-counties_NERCs_sub[, c('GEOID', 'NERC')] 

# Merge county-NERC info to county_results
county_results <- merge(county_results, counties_NERCs_sub, by = "GEOID", all.x = TRUE)

# 5. Aggregate EAGLE-I Metrics from County-level to NERC level ----
message("Aggregating EAGLE-I metrics to NERC subregion level...")
# Convert to data.table for efficient aggregation
county_results_dt <- as.data.table(county_results)

coverage_NERC <- county_results_dt[, .(
  total_customers = sum(county_total_customers, na.rm = TRUE),
  min_covered = sum(county_min_covered, na.rm = TRUE),
  max_covered = sum(county_max_covered, na.rm = TRUE)
), by = c("year", "NERC")]

# # Write out the EAGLE-I coverage by NERC region
# fwrite(coverage_NERC, eaglei_coverage_nerc_path)

# 6. Load and Prepare IEEE Customer Data ----
message("Loading and preparing IEEE customer data...")
IEEE <- fread(ieee_customers_path)

# Aggregate IEEE data from region level to NERC level
IEEE_NERC <- IEEE[, .(customers_IEEE = sum(Customers, na.rm = TRUE)), by = .(NERC, Year)]

# Extract IEEE data between 2018 and 2022 ()
IEEE_NERC <- IEEE_NERC[IEEE_NERC$Year >= 2018 & IEEE_NERC$Year <= 2022, ]

# Rename 'SPP RE' to 'SPP' for consistency with EAGLE-I data
IEEE_NERC[IEEE_NERC$NERC == "SPP RE", "NERC"] <- "SPP"
# Rename 'Year' to 'year' for consistency
colnames(IEEE_NERC)[colnames(IEEE_NERC) == "Year"] <- "year"

# 7. Join the Two Datasets ----
message("Joining EAGLE-I and IEEE datasets...")
joined <- merge(coverage_NERC, IEEE_NERC, by = c("NERC", "year"), all.x = TRUE)

# 8. Print National-level Summaries ----
message("\n--- National-level Customer Count Summaries ---")
for (yr in 2018:2022) {
  target <- coverage[coverage$year == yr, ]
  message(paste0("Sum of total customers in ", yr, " for the whole CONUS (EAGLE-I) is: ", sum(target$total_customers, na.rm = TRUE)))
  message(paste0("Sum of min covered customers in ", yr, " for the whole CONUS (EAGLE-I) is: ", sum(target$min_covered, na.rm = TRUE)))
  message(paste0("Sum of max covered customers in ", yr, " for the whole CONUS (EAGLE-I) is: ", sum(target$max_covered, na.rm = TRUE)))
  
  target_IEEE <- IEEE[IEEE$Year == yr, ]
  message(paste0("Sum of IEEE customers in ", yr, " for the whole CONUS is: ", sum(target_IEEE$Customers, na.rm = TRUE)))
}