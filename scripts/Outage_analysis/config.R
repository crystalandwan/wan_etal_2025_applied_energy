# config.R

# Load the 'here' package to enable relative paths
library(here)

# Define the file path to EAGLE-I coverage data
eaglei_coverage_path <- here("data", "EAGLE-I/coverage_history.csv")

# Define the file path to county-level population 2020data
county_pop_data_path <- here("data", "County_pop2020/nhgis0116_ds258_2020_county.csv")

# Define the file path to the shapefile depicting relationship between counties and NERC regions
counties_nerc_shp_path <- here("data", "County_NERC_shapefile/counties_in_NERC2020.shp")

# Define the file pah to timezone shapefile
time_zone_shp_path <- here("data", "Time_Zones/Time_Zones.shp")

# Define the file path to IEEE coverage data
ieee_customers_path <- here("data", "IEEE/customer counts.csv")

# Define the file path to EAGLE-I aggregation utility functions
eaglei_agg_script_path <- here("scripts", "Outage_analysis/Aggregate_eaglei_from_15min_to_daily.R")

# Define the folder path to EAGLE-I data
eaglei_raw_data_dir <- here("data", "EAGLE-I")

