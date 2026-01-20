# config.R

# Load the 'here' package to enable relative paths
library(here)

## --- EAGLE-I data path --- 
# Raw coverage data
eaglei_coverage_path <- here("data", "EAGLE-I/coverage_history.csv")

# Processed coverage data at NERC level
eaglei_coverage_nerc_path <- here("data", "Output/eaglei_coverage_by_NERC.csv")

# Raw outage data directory
eaglei_raw_data_dir <- here("data", "EAGLE-I")

# Processed outage data at the NERC level with time zone adjusted to local time
eaglei_nerc_time_adjusted_path <- here("data", "Output/eaglei_NERC_time_adjusted.csv")

# Calculated reliability metrics data
eaglei_reliability_path <- here("data", "Output/eaglei_rolling_reliability.csv")

## --- IEEE data path ---
# Raw coverage data
ieee_customers_path <- here("data", "IEEE/customer counts.csv")

# Raw outage data (including both MED- and non-MED days)
ieee_combined_path <- here("data", "IEEE/combined.csv")


# Calculated reliability metrics data
ieee_reliability_path <- here("data", "Output")


## --- Others ---
# Define the file path to county-level population 2020data
county_pop_data_path <- here("data", "County_pop2020/nhgis0116_ds258_2020_county.csv")

# Define the file path to the shapefile depicting relationship between counties and NERC regions
counties_nerc_shp_path <- here("data", "County_NERC_shapefile/counties_in_NERC2020.shp")

# Define the file pah to timezone shapefile
time_zone_shp_path <- here("data", "Time_Zones/Time_Zones.shp")

# Define the file path to EAGLE-I aggregation utility functions
eaglei_agg_script_path <- here("scripts", "Outage_analysis/Aggregate_eaglei_from_15min_to_daily.R")

# --- IEEE Versions to Process ---
# This list defines which IEEE outage datasets to process.
# Each element should contain:
#   - name: A descriptive name for the version (used in messages).
#   - path: The name of the variable in config.R that holds the full path to the outage data.
#   - output_suffix: A string to append to the base output filename (e.g., "_MED_excluded").
ieee_versions_to_process <- list(
  list(
    name = "MED Included",
    path = here("data", "IEEE/combined.csv"),
    output_suffix = "" # No suffix for the main version
  ),
  list(
    name = "MED Excluded",
    path = here("data", "IEEE/combined excl med.csv"),
    output_suffix = "_MED_excluded"
  )
)




