# *****************************************************************************************
# Title: Comparisons between IEEE and EAGLE-I
# Author: Heng Wan
# Date: 2025-10-30
# Purpose: This script performs a comparison of power outage metrics between EAGLE-I and 
#          IEEE data. It first unify the spatial and temporal resolution of the two
#          datasets, and then performs statistical analysis to assess the correlations
#          between the two datasets.
# Description:
#   1. Loads geographic data (county-NERC mapping, time zones) and assigns NERC
#      subregions and local time zones to each county.
#   2. Reads raw 15-minute EAGLE-I outage data across multiple years, converts
#      timestamps to local time, and applies time zone adjustments.
#   3. Aggregates the 15-minute EAGLE-I data to daily metrics at the county level
#      using three methods: maximum customer impact, sum of positive increases in
#      customer impact, and total customer-minutes.
#   4. Further aggregates the daily county-level EAGLE-I metrics to NERC subregion level.
#   5. Loads and processes daily IEEE outage data, including calculating 3-day
#      moving averages and joining with IEEE customer count data.
#   6. Merges the processed NERC-level EAGLE-I and IEEE datasets.
#   7. Filters the combined dataset to a common analysis period and applies a
#      threshold for customer impact percentage to focus on significant events.
#   8. Calculates Pearson and Spearman correlation coefficients between various
#      EAGLE-I and IEEE metrics.
#   9. Generates scatter plots and box plots to visually compare the different
#      aggregation methods and the two data sources, both at the national (all NERC)
#      and individual NERC region levels.
# Requirements:
#   - R packages: `data.table`, `dplyr`, `sf`, `zoo`, `lubridate`, `here`.
#   - A `config.R` file located in `scripts/Outage_Analysis/` relative to `here::here()`,
#     defining paths, years, and specific FIPS codes for time zone handling.
#   - A separate script `Aggregate_eaglei_from_15min_to_daily.R` containing aggregation
#     functions, located as specified in `config.R`.
#   - Data files:
#     - `counties_in_NERC2020.shp`: County-to-NERC mapping.
#     - `Time_Zones.shp`: Time zone boundaries.
#     - `eaglei_outages_YYYY.csv`: Raw 15-minute EAGLE-I data (for each year).
#     - `combined.csv`: Daily IEEE outage data (CI, CMI).
#     - `customer counts.csv`: IEEE customer counts by NERC region and year.
#     (All paths are configured in `config.R`)
# ******************************************************************************************

# Load Required Libraries ----
library(data.table) 
library(dplyr)      
library(sf)         
library(zoo)        
library(lubridate)  
library(here)       

# Source Configuration File ----
config_path <- here::here("scripts", "Outage_analysis", "config.R")
if (!file.exists(config_path)) {
  stop("The configuration file does not exist. Ensure the correct path:", config_path)
}
source(config_path)

# Source EAGLE-I Aggregation Functions ----
if (!file.exists(eaglei_agg_script_path)) {
  stop("The EAGLE-I aggregation script does not exist. Ensure the correct path:", eaglei_agg_script_path)
}
source(eaglei_agg_script_path)


# --- Main Script Logic ---

# 1. Prepare Geographic and Time Zone Data ----
message("1. Preparing geographic and time zone data...")

# Read in county-NERC mapping shapefile
counties_NERCs <- st_read(counties_nerc_shp_path) %>%
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
  ) %>%
  mutate(fips_code = as.numeric(GEOID)) # Create fips_code column for joining

# Get county centroids for faster spatial join with time zone shapefile
counties_NERCs_sub <- st_centroid(counties_NERCs_sub)

# Read in time zone shapefile
time_zone <- st_read(time_zone_shp_path) %>%
  st_transform(crs = st_crs(counties_NERCs)) %>% # Transform for consistent CRS
  st_make_valid() # Fix any invalid geometries

# Spatial join to assign time zones
counties_NERCs_sub <- st_join(counties_NERCs_sub, time_zone, join = st_intersects)

# There are two counties that are not handled by this spatial join. Manually assign the time zone
counties_NERCs_sub[counties_NERCs_sub$GEOID == "12087", "zone"] <- "Eastern"
counties_NERCs_sub[counties_NERCs_sub$GEOID == "12087", "utc"] <- "-05:00"

counties_NERCs_sub[counties_NERCs_sub$GEOID == "06075", "zone"] <- "Pacific"
counties_NERCs_sub[counties_NERCs_sub$GEOID == "06075", "utc"] <- "-08:00"

# Drop geometry
counties_NERCs_sub <- st_drop_geometry(counties_NERCs_sub)


# 2. Load and Pre-process EAGLE-I Raw Data ----
message("2. Loading and pre-processing raw EAGLE-I data (15-min interval)...")

# Get list of EAGLE-I data files
eaglei_file_paths <- list.files(
  path = eaglei_raw_data_dir,
  pattern = paste0("^", "eaglei_outages_", "(\\d{4})", ".csv$"),
  full.names = TRUE
)

if (length(eaglei_file_paths) == 0) {
  stop("No EAGLE-I raw data files found for the specified years. Check paths and year range in config.R.")
}

# Read all EAGLE-I files and combine them
eaglei_list <- lapply(eaglei_file_paths, fread)
eaglei_all <- rbindlist(eaglei_list)

# Join time zone info to EAGLE-I data
eaglei_all <- merge(eaglei_all, counties_NERCs_sub[, c("fips_code", "zone")])





# Map simplified time zone names to IANA time zone identifiers and adjust for Arizona
eaglei_all <- eaglei_all %>%
  mutate(
    tz_to_apply = case_when(
      fips_code %in% arizona_fips_codes & fips_code != navajo_nation_fips & zone == "Mountain" ~ "America/Phoenix", # No DST
      fips_code == navajo_nation_fips & zone == "Mountain" ~ "America/Denver", # Navajo Nation uses DST
      TRUE ~ iana_tz_map[zone] # Default to mapped IANA TZ
    )
  )

# Convert `run_start_time` to POSIXct object first
eaglei_all$run_start_time <- ymd_hms(eaglei_all$run_start_time, tz = "UTC")

# Adjust the UTC time to local time for each county
eaglei_all[, local_time := with_tz(run_start_time, tzone = tz_to_apply), by = .(fips_code)]

# Rename columns for compatibility with aggregation functions
# The aggregation functions expect `run_start_time` to be the local time.
colnames(eaglei_all)[which(colnames(eaglei_all) == "run_start_time")] <- "run_start_time_UTC"
colnames(eaglei_all)[which(colnames(eaglei_all) == "local_time")] <- "run_start_time"


# 3. Aggregate EAGLE-I Data to Daily and NERC Level ----
message("3. Aggregating EAGLE-I data to daily and NERC levels...")

# Aggregate data from 15min to daily by taking the max
eaglei_daily_max <- aggregate_by_max_count(eaglei_all)

# Aggregate data from 15min to daily by summing positive differences
eaglei_daily_sum_positive_diff <- aggregate_by_sum_positive_diff(eaglei_all)

# Calculate daily customer minutes
eaglei_daily_customer_minutes <- calculate_customer_minutes(eaglei_all)

# Join the 3 daily datasets
eaglei_daily <- merge(eaglei_daily_sum_positive_diff, eaglei_daily_customer_minutes, by = c("fips_code", "Date"), all.x = TRUE)
eaglei_daily <- merge(eaglei_daily, eaglei_daily_max, by = c("fips_code", "Date"), all.x = TRUE)

# Fill NA values as 0 for total_customer_minutes and max_customer where no outage was reported
eaglei_daily <- eaglei_daily %>%
  mutate(
    total_customer_minutes = ifelse(is.na(total_customer_minutes), 0, total_customer_minutes),
    max_customer = ifelse(is.na(max_customer), 0, max_customer)
  ) %>%
  arrange(fips_code, Date)

# Remove the first and last days of the overall period for which 3-day moving averages
# might be incomplete or inaccurate due to boundary conditions.
# The `aggregate_by_sum_positive_diff` already handles `rollmean` `fill=NA`, so this
# is more about removing incomplete daily windows from the entire dataset boundary.
# The original script had specific dates "2014-11-01", "2022-11-12".
# This should be dynamic based on the actual min/max dates after aggregation.
min_date_for_avg <- min(eaglei_daily$Date, na.rm = TRUE) + 1 # First valid day for 3-day avg center
max_date_for_avg <- max(eaglei_daily$Date, na.rm = TRUE) - 1 # Last valid day for 3-day avg center
eaglei_daily <- eaglei_daily[eaglei_daily$Date >= min_date_for_avg & eaglei_daily$Date <= max_date_for_avg, ]

# Join county-NERC info to EAGLE-I daily data
eaglei_daily <- merge(eaglei_daily, counties_tz_info[, c("fips_code", "NERC")], by = "fips_code", all.x = FALSE)

# Convert to data.table for efficient NERC-level aggregation
eaglei_daily_dt <- as.data.table(eaglei_daily)

# Aggregate to NERC region-level
eaglei_NERC_aggregated <- eaglei_daily_dt[, .(
  max_customer = sum(max_customer, na.rm = TRUE),
  daily_ci = sum(daily_ci, na.rm = TRUE),
  daily_ci_3day_avg = sum(daily_ci_3day_avg, na.rm = TRUE),
  customer_minutes = sum(total_customer_minutes, na.rm = TRUE)
), by = .(NERC, Date)]

# Remove NERC regions that might be NA if some counties couldn't be mapped
eaglei_NERC_aggregated <- eaglei_NERC_aggregated[!is.na(NERC)]

# Optional: Write out the aggregated NERC-level EAGLE-I data
if (!is.null(eaglei_nerc_output_path)) {
  # Ensure the output directory exists
  output_dir <- dirname(eaglei_nerc_output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  fwrite(eaglei_NERC_aggregated, eaglei_nerc_output_path)
  message(paste0("Aggregated EAGLE-I NERC data saved to: ", eaglei_nerc_output_path))
} else {
  message("Skipping saving aggregated EAGLE-I NERC data (eaglei_nerc_output_path not defined).")
}

# Read back for consistency (if saved and re-read is desired, otherwise use eaglei_NERC_aggregated directly)
# This assumes the file structure on disk is consistent.
eaglei_combined_NERC <- fread(eaglei_nerc_output_path)


# 4. Load and Prepare IEEE Dataset ----
message("4. Loading and preparing IEEE dataset...")

# Read in IEEE combined outage data
IEEE_combined <- fread(ieee_combined_path)

# Rename "SPP RE" to "SPP" for consistency
IEEE_combined[IEEE_combined$NERC == "SPP RE", "NERC"] <- "SPP"

# Aggregate to NERC level (if not already fully aggregated, and ensure correct column names)
# The `combined.csv` is usually already aggregated to NERC level, so this might be redundant
# but ensures summing if there are any finer-grained records.
IEEE_combined_NERC <- IEEE_combined[, .(CI = sum(CI, na.rm = TRUE), CMI = sum(CMI, na.rm = TRUE)),
                                    by = .(NERC, Date)]

# Calculate 3-day moving window average for IEEE CI
IEEE_combined_NERC <- IEEE_combined_NERC %>%
  mutate(Date = as.Date(Date)) %>% # Ensure Date is in Date format for sorting
  arrange(NERC, Date) %>%
  group_by(NERC) %>%
  mutate(CI_moving = rollmean(CI, k = 3, fill = NA, align = "center")) %>% # Corrected 'aligh' to 'align'
  ungroup()

# Remove the dates with NA values in CI_moving (boundary conditions for moving average)
IEEE_combined_NERC <- IEEE_combined_NERC[!is.na(IEEE_combined_NERC$CI_moving), ]

# Add a year column
IEEE_combined_NERC$year <- year(IEEE_combined_NERC$Date)

# Read in IEEE customer data
IEEE_customer <- fread(ieee_customer_path)

# Rename "SPP RE" to "SPP" for consistency
IEEE_customer[IEEE_customer$NERC == "SPP RE", "NERC"] <- "SPP"

# Aggregate IEEE customer data from region level to NERC level
IEEE_customer_NERC <- IEEE_customer[, .(customers_IEEE = sum(Customers, na.rm = TRUE)), by = .(NERC, Year)]

# Join customer count to outage data
IEEE_combined_NERC <- merge(IEEE_combined_NERC, IEEE_customer_NERC,
                            by.x = c("year", "NERC"), by.y = c("Year", "NERC"), all.x = TRUE)


# 5. Merge EAGLE-I and IEEE Datasets ----
message("5. Merging EAGLE-I and IEEE datasets for comparison...")

# Merge the two datasets on NERC and Date
Merged_data <- merge(IEEE_combined_NERC, eaglei_combined_NERC,
                     by = c("NERC", "Date"), all.x = TRUE, all.y = TRUE)

# Calculate the customers impacted percentage
Merged_data$CI_per <- Merged_data$CI / Merged_data$customers_IEEE * 100

# Extract records with overlapping time period defined in config.R
Merged_data_filtered <- Merged_data[
  Merged_data$Date >= analysis_start_date & Merged_data$Date <= analysis_end_date,
]

# Obtain records with both valid CI metrics from IEEE and EAGLE-I
valid_comparison_data <- Merged_data_filtered[
  !is.na(Merged_data_filtered$CI) & !is.na(Merged_data_filtered$daily_ci_3day_avg),
]


# 6. Initial Exploration and Filtering ----
message("6. Exploring and filtering data...")

# Explore CI_per distribution
breaks_ci_per <- c(0, 0.25, 0.5, 1, 2, 5, 10, 20, 100, Inf) # Added Inf for the last bin
valid_comparison_data$CI_per_Range <- cut(valid_comparison_data$CI_per,
                                          breaks = breaks_ci_per,
                                          right = TRUE, include.lowest = TRUE)
message("\nDistribution of CI_per ranges:")
print(table(valid_comparison_data$CI_per_Range))

# Histogram of CI_per
hist(valid_comparison_data$CI_per, breaks = 1000, xlim = c(0, 10),
     xlab = "Percent of Customer Impact (CI)",
     main = "Histogram of Percent of CI (2018 - 2022)")

# Filter the data by removing low CI% based on `min_ci_percentage_filter` from config.R
valid_comparison_data <- valid_comparison_data[valid_comparison_data$CI_per > min_ci_percentage_filter, ]
message(paste0("Filtered data to include only records with CI_per > ", min_ci_percentage_filter, "%"))


# 7. Correlation Analysis ----
message("\n7. Performing correlation analysis...")

message("\nPearson's correlation coefficients:")
message(paste0("  CI (IEEE) vs. max_customer (EAGLE-I): ", round(cor(valid_comparison_data$CI, valid_comparison_data$max_customer, use = "pairwise.complete.obs"), 3)))
message(paste0("  CMI (IEEE) vs. customer_minutes (EAGLE-I): ", round(cor(valid_comparison_data$CMI, valid_comparison_data$customer_minutes, use = "pairwise.complete.obs"), 3)))
message(paste0("  CI (IEEE) vs. daily_ci (EAGLE-I): ", round(cor(valid_comparison_data$CI, valid_comparison_data$daily_ci, use = "pairwise.complete.obs"), 3)))
message(paste0("  CI (IEEE) vs. daily_ci_3day_avg (EAGLE-I): ", round(cor(valid_comparison_data$CI, valid_comparison_data$daily_ci_3day_avg, use = "pairwise.complete.obs"), 3)))
message(paste0("  CI_moving (IEEE) vs. daily_ci_3day_avg (EAGLE-I): ", round(cor(valid_comparison_data$CI_moving, valid_comparison_data$daily_ci_3day_avg, use = "pairwise.complete.obs"), 3)))

message("\nSpearman's rank correlation coefficients:")
message(paste0("  CI (IEEE) vs. max_customer (EAGLE-I): ", round(cor(valid_comparison_data$CI, valid_comparison_data$max_customer, method = "spearman", use = "pairwise.complete.obs"), 3)))
message(paste0("  CMI (IEEE) vs. customer_minutes (EAGLE-I): ", round(cor(valid_comparison_data$CMI, valid_comparison_data$customer_minutes, method = "spearman", use = "pairwise.complete.obs"), 3)))
message(paste0("  CI (IEEE) vs. daily_ci (EAGLE-I): ", round(cor(valid_comparison_data$CI, valid_comparison_data$daily_ci, method = "spearman", use = "pairwise.complete.obs"), 3)))
message(paste0("  CI (IEEE) vs. daily_ci_3day_avg (EAGLE-I): ", round(cor(valid_comparison_data$CI, valid_comparison_data$daily_ci_3day_avg, method = "spearman", use = "pairwise.complete.obs"), 3)))
message(paste0("  CI_moving (IEEE) vs. daily_ci_3day_avg (EAGLE-I): ", round(cor(valid_comparison_data$CI_moving, valid_comparison_data$daily_ci_3day_avg, method = "spearman", use = "pairwise.complete.obs"), 3)))


# 8. Visualization of Comparisons (All NERC Regions) ----
message("\n8. Generating visualizations for all NERC regions...")

# Convert NERC column to factor for consistent plotting colors
valid_comparison_data$NERC_factor <- as.factor(valid_comparison_data$NERC)
valid_comparison_data$year_factor <- as.factor(valid_comparison_data$year)

# Comparison between EAGLE-I aggregation methods (Daily Max vs. Sum Positive Diff)
plot(valid_comparison_data$max_customer / 1000, valid_comparison_data$daily_ci / 1000,
     xlab = "EAGLE-I Daily Max CI (in thousands)",
     ylab = "EAGLE-I Sum of Positive Diff CI (in thousands)",
     main = "EAGLE-I CI Comparison by Aggregation Method (All NERC)",
     col = valid_comparison_data$NERC_factor,
     pch = 19, cex = 0.8)
abline(a = 0, b = 1, col = "red", lty = 2, lwd = 1.5)
legend("topleft", legend = levels(valid_comparison_data$NERC_factor),
       col = unique(valid_comparison_data$NERC_factor), pch = 19, title = "NERC Regions",
       cex = 0.7, bty = "n")

# Boxplot of Log(CI) for different EAGLE-I aggregation methods
boxplot(log(valid_comparison_data$max_customer + 1), # Add 1 to handle 0 values for log
        log(valid_comparison_data$daily_ci + 1),
        log(valid_comparison_data$daily_ci_3day_avg + 1),
        names = c("EAGLE-I_daily_max", "EAGLE-I_sum_positive_diff", "EAGLE-I_Moving_avg"),
        ylab = "Log(Customer Impact + 1)",
        main = "Distribution of Log(CI) by EAGLE-I Aggregation Method",
        las = 1, cex.axis = 0.8)

# (IEEE CI vs. EAGLE-I Daily Max)
plot(valid_comparison_data$CI / 1000, valid_comparison_data$max_customer / 1000,
     xlab = "IEEE CI (in thousands)",
     ylab = "EAGLE-I Daily Max CI (in thousands)",
     xlim = c(0, max(valid_comparison_data$CI, valid_comparison_data$max_customer, na.rm = TRUE) / 1000),
     ylim = c(0, max(valid_comparison_data$CI, valid_comparison_data$max_customer, na.rm = TRUE) / 1000),
     col = valid_comparison_data$NERC_factor,
     main = "CI Comparison: IEEE vs. EAGLE-I Daily Max (All NERC)",
     cex.main = 1.5, cex.lab = 1.2, pch = 19, cex = 0.8)
abline(a = 0, b = 1, col = "red", lty = 2, lwd = 1.5)
legend("topleft", legend = levels(valid_comparison_data$NERC_factor),
       col = unique(valid_comparison_data$NERC_factor), pch = 19, title = "NERC Regions",
       cex = 0.7, bty = "n")

# Boxplot of Log(CI) for IEEE vs. EAGLE-I aggregation methods
boxplot(log(valid_comparison_data$CI + 1), log(valid_comparison_data$CI_moving + 1),
        log(valid_comparison_data$max_customer + 1), log(valid_comparison_data$daily_ci + 1),
        log(valid_comparison_data$daily_ci_3day_avg + 1),
        names = c("IEEE", "IEEE_moving_avg", "EAGLE-I_daily_max", "EAGLE-I_sum_positive_diff",
                  "EAGLE-I_Moving_avg"),
        ylab = "Log(Customer Impact + 1)",
        main = "Distribution of Log(CI) Across All Metrics",
        las = 2, cex.axis = 0.8) # las=2 for vertical labels

# (IEEE CI vs. EAGLE-I Sum Positive Diff)
plot(valid_comparison_data$CI / 1000, valid_comparison_data$daily_ci / 1000,
     xlab = "IEEE CI (in thousands)",
     ylab = "EAGLE-I Sum of Positive Diff CI (in thousands)",
     xlim = c(0, max(valid_comparison_data$CI, valid_comparison_data$daily_ci, na.rm = TRUE) / 1000),
     ylim = c(0, max(valid_comparison_data$CI, valid_comparison_data$daily_ci, na.rm = TRUE) / 1000),
     col = valid_comparison_data$NERC_factor,
     main = "CI Comparison: IEEE vs. EAGLE-I Sum Positive Diff (All NERC)",
     cex.main = 1.5, cex.lab = 1.2, pch = 19, cex = 0.8)
abline(a = 0, b = 1, col = "red", lty = 2, lwd = 1.5)
legend("topleft", legend = levels(valid_comparison_data$NERC_factor),
       col = unique(valid_comparison_data$NERC_factor), pch = 19, title = "NERC Regions",
       cex = 0.7, bty = "n")

# (IEEE CI vs. EAGLE-I 3-day Moving Avg)
plot(valid_comparison_data$CI / 1000, valid_comparison_data$daily_ci_3day_avg / 1000,
     xlab = "IEEE CI (in thousands)",
     ylab = "EAGLE-I 3-day Moving Average CI (in thousands)",
     xlim = c(0, max(valid_comparison_data$CI, valid_comparison_data$daily_ci_3day_avg, na.rm = TRUE) / 1000),
     ylim = c(0, max(valid_comparison_data$CI, valid_comparison_data$daily_ci_3day_avg, na.rm = TRUE) / 1000),
     col = valid_comparison_data$NERC_factor,
     main = "CI Comparison: IEEE vs. EAGLE-I 3-day MA (All NERC)",
     cex.main = 1.5, cex.lab = 1.2, pch = 19, cex = 0.8)
abline(a = 0, b = 1, col = "red", lty = 2, lwd = 1.5)
legend("topleft", legend = levels(valid_comparison_data$NERC_factor),
       col = unique(valid_comparison_data$NERC_factor), pch = 19, title = "NERC Regions",
       cex = 0.7, bty = "n")

# (IEEE 3-day Moving Avg vs. EAGLE-I 3-day Moving Avg)
plot(valid_comparison_data$CI_moving / 1000, valid_comparison_data$daily_ci_3day_avg / 1000,
     xlab = "IEEE 3-day Moving Average CI (in thousands)",
     ylab = "EAGLE-I 3-day Moving Average CI (in thousands)",
     xlim = c(0, max(valid_comparison_data$CI_moving, valid_comparison_data$daily_ci_3day_avg, na.rm = TRUE) / 1000),
     ylim = c(0, max(valid_comparison_data$CI_moving, valid_comparison_data$daily_ci_3day_avg, na.rm = TRUE) / 1000),
     col = valid_comparison_data$NERC_factor,
     main = "CI Comparison: IEEE 3-day MA vs. EAGLE-I 3-day MA (All NERC)",
     cex.main = 1.5, cex.lab = 1.2, pch = 19, cex = 0.8)
abline(a = 0, b = 1, col = "red", lty = 2, lwd = 1.5)
legend("topleft", legend = levels(valid_comparison_data$NERC_factor),
       col = unique(valid_comparison_data$NERC_factor), pch = 19, title = "NERC Regions",
       cex = 0.7, bty = "n")

# (CMI comparisons)
plot(valid_comparison_data$CMI / 1000000, valid_comparison_data$customer_minutes / 1000000,
     xlab = "IEEE CMI (in millions)",
     ylab = "EAGLE-I CMI (in millions)",
     xlim = c(0, max(valid_comparison_data$CMI, valid_comparison_data$customer_minutes, na.rm = TRUE) / 1000000),
     ylim = c(0, max(valid_comparison_data$CMI, valid_comparison_data$customer_minutes, na.rm = TRUE) / 1000000),
     col = valid_comparison_data$NERC_factor,
     main = "CMI Comparison: IEEE vs. EAGLE-I (All NERC)",
     cex.main = 1.5, cex.lab = 1.2, pch = 19, cex = 0.8)
abline(a = 0, b = 1, col = "red", lty = 2, lwd = 1.5)
legend("topleft", legend = levels(valid_comparison_data$NERC_factor),
       col = unique(valid_comparison_data$NERC_factor), pch = 19, title = "NERC Regions",
       cex = 0.7, bty = "n")

# Boxplot of Log(CMI) for IEEE vs. EAGLE-I
boxplot(log(valid_comparison_data$CMI + 1), log(valid_comparison_data$customer_minutes + 1),
        names = c("IEEE CMI", "EAGLE-I CMI"),
        ylab = "Log(Customer Minutes Impact + 1)",
        main = "Distribution of Log(CMI) Across Metrics",
        las = 1, cex.axis = 0.8)


# 9. Visualization of Comparisons (Individual NERC Region) ----
message("\n9. Generating visualizations for an individual NERC region (e.g., TRE)...")

# Choose a NERC region for detailed comparison (e.g., "TRE")
selected_nerc_region <- "TRE"
target_nerc_data <- valid_comparison_data[valid_comparison_data$NERC == selected_nerc_region, ]

if (nrow(target_nerc_data) == 0) {
  message(paste0("No data found for NERC region: ", selected_nerc_region, ". Skipping individual NERC plots."))
} else {
  message(paste0("  Analysis for NERC region: ", selected_nerc_region))
  
  # (CI vs. Daily Max)
  message(paste0("    Correlations for ", selected_nerc_region, " (CI vs. Daily Max):"))
  message(paste0("      Pearson: ", round(cor(target_nerc_data$CI, target_nerc_data$max_customer, use = "pairwise.complete.obs"), 3)))
  message(paste0("      Spearman: ", round(cor(target_nerc_data$CI, target_nerc_data$max_customer, method = "spearman", use = "pairwise.complete.obs"), 3)))
  plot(target_nerc_data$CI / 1000, target_nerc_data$max_customer / 1000,
       xlab = "IEEE CI (in thousands)",
       ylab = "EAGLE-I Daily Max CI (in thousands)",
       xlim = c(0, max(target_nerc_data$CI, target_nerc_data$max_customer, na.rm = TRUE) / 1000),
       ylim = c(0, max(target_nerc_data$CI, target_nerc_data$max_customer, na.rm = TRUE) / 1000),
       col = target_nerc_data$year_factor,
       main = paste0("CI Comparison: IEEE vs. EAGLE-I Daily Max (", selected_nerc_region, ")"),
       pch = 19, cex = 0.8)
  abline(a = 0, b = 1, col = "red", lty = 2, lwd = 1.5)
  legend("topleft", legend = levels(target_nerc_data$year_factor),
         col = unique(target_nerc_data$year_factor), pch = 19, title = "Years",
         cex = 0.7, bty = "n")
  boxplot(log(target_nerc_data$CI + 1), log(target_nerc_data$max_customer + 1),
          names = c("IEEE CI", "EAGLE-I Daily Max CI"),
          ylab = "Log(Customer Impact + 1)",
          main = paste0("Log(CI) Distribution (", selected_nerc_region, ")"),
          las = 1, cex.axis = 0.8)
  
  # (CI vs. Sum Positive Diff)
  message(paste0("    Correlations for ", selected_nerc_region, " (CI vs. Sum Positive Diff):"))
  message(paste0("      Pearson: ", round(cor(target_nerc_data$CI, target_nerc_data$daily_ci, use = "pairwise.complete.obs"), 3)))
  message(paste0("      Spearman: ", round(cor(target_nerc_data$CI, target_nerc_data$daily_ci, method = "spearman", use = "pairwise.complete.obs"), 3)))
  plot(target_nerc_data$CI / 1000, target_nerc_data$daily_ci / 1000,
       xlab = "IEEE CI (in thousands)",
       ylab = "EAGLE-I Sum of Positive Diff CI (in thousands)",
       xlim = c(0, max(target_nerc_data$CI, target_nerc_data$daily_ci, na.rm = TRUE) / 1000),
       ylim = c(0, max(target_nerc_data$CI, target_nerc_data$daily_ci, na.rm = TRUE) / 1000),
       col = target_nerc_data$year_factor,
       main = paste0("CI Comparison: IEEE vs. EAGLE-I Sum Positive Diff (", selected_nerc_region, ")"),
       pch = 19, cex = 0.8)
  abline(a = 0, b = 1, col = "red", lty = 2, lwd = 1.5)
  legend("topleft", legend = levels(target_nerc_data$year_factor),
         col = unique(target_nerc_data$year_factor), pch = 19, title = "Years",
         cex = 0.7, bty = "n")
  
  # (CI vs. Moving Avg)
  message(paste0("    Correlations for ", selected_nerc_region, " (CI vs. Moving Avg):"))
  message(paste0("      Pearson: ", round(cor(target_nerc_data$CI, target_nerc_data$daily_ci_3day_avg, use = "pairwise.complete.obs"), 3)))
  message(paste0("      Spearman: ", round(cor(target_nerc_data$CI, target_nerc_data$daily_ci_3day_avg, method = "spearman", use = "pairwise.complete.obs"), 3)))
  plot(target_nerc_data$CI / 1000, target_nerc_data$daily_ci_3day_avg / 1000,
       xlab = "IEEE CI (in thousands)",
       ylab = "EAGLE-I 3-day Moving Average CI (in thousands)",
       xlim = c(0, max(target_nerc_data$CI, target_nerc_data$daily_ci_3day_avg, na.rm = TRUE) / 1000),
       ylim = c(0, max(target_nerc_data$CI, target_nerc_data$daily_ci_3day_avg, na.rm = TRUE) / 1000),
       col = target_nerc_data$year_factor,
       main = paste0("CI Comparison: IEEE vs. EAGLE-I 3-day MA (", selected_nerc_region, ")"),
       pch = 19, cex = 0.8)
  abline(a = 0, b = 1, col = "red", lty = 2, lwd = 1.5)
  legend("topleft", legend = levels(target_nerc_data$year_factor),
         col = unique(target_nerc_data$year_factor), pch = 19, title = "Years",
         cex = 0.7, bty = "n")
  
  # (IEEE CI Moving Avg vs. EAGLE-I Moving Avg)
  message(paste0("    Correlations for ", selected_nerc_region, " (IEEE MA vs. EAGLE-I MA):"))
  message(paste0("      Pearson: ", round(cor(target_nerc_data$CI_moving, target_nerc_data$daily_ci_3day_avg, use = "pairwise.complete.obs"), 3)))
  message(paste0("      Spearman: ", round(cor(target_nerc_data$CI_moving, target_nerc_data$daily_ci_3day_avg, method = "spearman", use = "pairwise.complete.obs"), 3)))
  plot(target_nerc_data$CI_moving / 1000, target_nerc_data$daily_ci_3day_avg / 1000,
       xlab = "IEEE 3-day Moving Average CI (in thousands)",
       ylab = "EAGLE-I 3-day Moving Average CI (in thousands)",
       xlim = c(0, max(target_nerc_data$CI_moving, target_nerc_data$daily_ci_3day_avg, na.rm = TRUE) / 1000),
       ylim = c(0, max(target_nerc_data$CI_moving, target_nerc_data$daily_ci_3day_avg, na.rm = TRUE) / 1000),
       col = target_nerc_data$year_factor,
       main = paste0("CI Comparison: IEEE 3-day MA vs. EAGLE-I 3-day MA (", selected_nerc_region, ")"),
       pch = 19, cex = 0.8)
  abline(a = 0, b = 1, col = "red", lty = 2, lwd = 1.5)
  legend("topleft", legend = levels(target_nerc_data$year_factor),
         col = unique(target_nerc_data$year_factor), pch = 19, title = "Years",
         cex = 0.7, bty = "n")
  
  # (CMI comparisons)
  message(paste0("    Correlations for ", selected_nerc_region, " (CMI comparison):"))
  message(paste0("      Pearson: ", round(cor(target_nerc_data$CMI, target_nerc_data$customer_minutes, use = "pairwise.complete.obs"), 3)))
  message(paste0("      Spearman: ", round(cor(target_nerc_data$CMI, target_nerc_data$customer_minutes, method = "spearman", use = "pairwise.complete.obs"), 3)))
  plot(target_nerc_data$CMI / 1000000, target_nerc_data$customer_minutes / 1000000,
       xlab = "IEEE CMI (in millions)",
       ylab = "EAGLE-I CMI (in millions)",
       xlim = c(0, max(target_nerc_data$CMI, target_nerc_data$customer_minutes, na.rm = TRUE) / 1000000),
       ylim = c(0, max(target_nerc_data$CMI, target_nerc_data$customer_minutes, na.rm = TRUE) / 1000000),
       col = target_nerc_data$year_factor,
       main = paste0("CMI Comparison: IEEE vs. EAGLE-I (", selected_nerc_region, ")"),
       pch = 19, cex = 0.8)
  abline(a = 0, b = 1, col = "red", lty = 2, lwd = 1.5)
  legend("topleft", legend = levels(target_nerc_data$year_factor),
         col = unique(target_nerc_data$year_factor), pch = 19, title = "Years",
         cex = 0.7, bty = "n")
  boxplot(log(target_nerc_data$CMI + 1), log(target_nerc_data$customer_minutes + 1),
          names = c("IEEE CMI", "EAGLE-I CMI"),
          ylab = "Log(Customer Minutes Impact + 1)",
          main = paste0("Log(CMI) Distribution (", selected_nerc_region, ")"),
          las = 1, cex.axis = 0.8)
}

message("\nScript execution complete.")