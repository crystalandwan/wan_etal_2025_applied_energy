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
counties_NERCs <- st_centroid(counties_NERCs)

# Read in time zone shapefile
time_zone <- st_read(time_zone_shp_path) %>%
  st_transform(crs = st_crs(counties_NERCs)) %>% # Transform for consistent CRS
  st_make_valid() # Fix any invalid geometries

# Spatial join to assign time zones
counties_NERCs <- st_join(counties_NERCs, time_zone, join = st_intersects)

# There are two counties that are not handled by this spatial join. Manually assign the time zone
counties_NERCs[counties_NERCs$GEOID == "12087", "zone"] <- "Eastern"
counties_NERCs[counties_NERCs$GEOID == "12087", "utc"] <- "-05:00"

counties_NERCs[counties_NERCs$GEOID == "06075", "zone"] <- "Pacific"
counties_NERCs[counties_NERCs$GEOID == "06075", "utc"] <- "-08:00"

# Drop geometry
counties_NERCs <- st_drop_geometry(counties_NERCs)


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
eaglei_all <- merge(eaglei_all, counties_NERCs[, c("fips_code", "zone")])

# Map simplified time zone names to IANA time zone identifiers
tz_mapping <- data.frame(
  zone = c("Eastern", "Central", "Mountain", "Pacific"),
  tz = c("America/New_York", "America/Chicago", "America/Denver", "America/Los_Angeles")
)

# Adjust the UTC time to local time for each county
# Handle Arizona counties (Mountain Time, no DST except Navajo Nation)
arizona_counties <- c(
  "4001", "4003", "4005", "4007", "4009", "4011", "4012",
  "4013", "4015", "4017", "4019", "4021", "4023", "4025", "4027"
) # All AZ FIPS codes
eaglei_all <- eaglei_all %>%
  mutate(
    tz = case_when(
      fips_code %in% arizona_counties & fips_code != "4017" & zone == "Mountain" ~ "America/Phoenix", # No DST
      fips_code == "4017" & zone == "Mountain" ~ "America/Denver", # Navajo Nation uses DST
      TRUE ~ tz_mapping$tz[match(zone, tz_mapping$zone)]
    )
  )

# Convert to local time
eaglei_all[, local_time := with_tz(run_start_time, tz), by = .(fips_code, tz)]

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

# Remove the first day (2014-11-01) and the last day (2022-11-12) as 3-day moving average will not work
eaglei_daily <- eaglei_daily[!eaglei_daily$Date %in% c("2014-11-01", "2022-11-12"), ]

# Join county-NERC info to EAGLE-I
eaglei_daily <- merge(eaglei_daily, counties_NERCs_sub[, c("fips_code", "NERC")], by = "fips_code")

# Convert to data table
eaglei_daily <- as.data.table(eaglei_daily)

# Aggregate to NERC region-level
eaglei_NERC <- eaglei_daily[, .(max_customer = sum(max_customer, na.rm = TRUE),
                                daily_ci = sum(daily_ci, na.rm = TRUE),
                                daily_ci_3day_avg = sum(daily_ci_3day_avg, na.rm = TRUE),
                                customer_minutes = sum(total_customer_minutes, na.rm = TRUE)),
                            by = .(NERC, Date)]

# # Write out the NERC-level EAGLE-I with time adjusted to local time
# fwrite(eaglei_NERC, eaglei_nerc_time_adjusted_path)


# 4. Load and Prepare IEEE Dataset ----
message("4. Loading and preparing IEEE dataset...")

# Read in IEEE combined outage data
IEEE_combined <- fread(ieee_combined_path)

# Rename "SPP RE" to "SPP" for consistency
IEEE_combined[IEEE_combined$NERC == "SPP RE", "NERC"] <- "SPP"

# Aggregate to NERC level 
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
IEEE_customer <- fread(ieee_customers_path)

# Rename "SPP RE" to "SPP" for consistency
IEEE_customer[IEEE_customer$NERC == "SPP RE", "NERC"] <- "SPP"

# Aggregate IEEE customer data from region level to NERC level
IEEE_customer_NERC <- IEEE_customer[, .(customers_IEEE = sum(Customers, na.rm = TRUE)), by = .(NERC, Year)]

# Join customer count to outage data
IEEE_combined_NERC <- merge(IEEE_combined_NERC, IEEE_customer_NERC,
                            by.x = c("year", "NERC"), by.y = c("Year", "NERC"), all.x = TRUE)


# 5. Merge EAGLE-I and IEEE Datasets ----
message("5. Merging EAGLE-I and IEEE datasets for comparison...")

# Merge the two datasets 
Merged <- merge(IEEE_combined_NERC, eaglei_combined_NERC, by = c("NERC", "Date"), all.x = TRUE, all.y = TRUE)

# Calculate the customers impacted percentage
Merged$CI_per <- Merged$CI / Merged$customers_IEEE * 100

# Extract records with overlapping time period
Merged_2018_2022 <- Merged[Merged$Date >= "2018-01-01" & Merged$Date <= "2022-11-11", ]

# Obtain records with both valid records
valid_2018_2022 <- Merged_2018_2022[!is.na(Merged_2018_2022$CI) & !is.na(Merged_2018_2022$daily_ci_3day_avg), ]

# Explore CI_per
breaks = c(0, 0.25, 0.5, 1, 2, 5, 10, 20, 100, 200)
valid_2018_2022$Range <- cut(valid_2018_2022$CI_per, breaks = breaks, right = TRUE, include.lowest = TRUE)
table(valid_2018_2022$Range)
hist(valid_2018_2022$CI_per, breaks = 1000, xlim = c(0, 10), xlab = "Percent of CI", 
     main = "Histogram of Percent of CI (2018 - 2022)")

# Filter the data frame by removing low CI%
valid_2018_2022 <- valid_2018_2022[valid_2018_2022$CI_per > 0.25, ]


# Pearson's correlation coefficient between the two datasets
cor(valid_2018_2022$CI, valid_2018_2022$max_customer)
cor(valid_2018_2022$CMI, valid_2018_2022$customer_minutes)
cor(valid_2018_2022$CI, valid_2018_2022$daily_ci)
cor(valid_2018_2022$CI, valid_2018_2022$daily_ci_3day_avg)
cor(valid_2018_2022$CI_moving, valid_2018_2022$daily_ci_3day_avg)

# Spearman's rank correlation coefficient between the two datasets
cor(valid_2018_2022$CI, valid_2018_2022$max_customer, method = "spearman")
cor(valid_2018_2022$CMI, valid_2018_2022$customer_minutes, method = "spearman")
cor(valid_2018_2022$CI, valid_2018_2022$daily_ci, method = "spearman")
cor(valid_2018_2022$CI, valid_2018_2022$daily_ci_3day_avg, method = "spearman")
cor(valid_2018_2022$CI_moving, valid_2018_2022$daily_ci_3day_avg, method = "spearman")

# Comparison between the aggregation methods
plot(valid_2018_2022$max_customer/1000, valid_2018_2022$daily_ci/1000,
     xlab = "Daily max (in thousands)",
     ylab = "Sum_positive_diff (in thousands)",
     # xlim = c(0, 9000),
     # ylim = c(0, 9000),
     col = as.factor(valid_2018_2022$NERC),
     main = "CI comparison for all NERC regions by aggregation method")
abline(a = 0, b = 1,
       col = "red",        # Set line color (e.g., red)
       lty = 2,            # Set line type (e.g., 2 for dashed)
       lwd = 1.5)          # Set line width (optional)
legend("topright", legend = unique(as.factor(valid_2018_2022$NERC)), 
       col = unique(as.factor(valid_2018_2022$NERC)), pch = 1, title = "NERC Regions")

boxplot(log(valid_2018_2022$max_customer), 
        log(valid_2018_2022$daily_ci), 
        log(valid_2018_2022$daily_ci_3day_avg),
        names = c("EAGLE-I_daily_max", "EAGLE-I_sum_positive_diff", "EAGLE-I_Moving_avg"), 
        ylab = "Log (CI)", 
        las = 1)


## Compare the two outage datasets for all NERC regions
# (IEEE CI v.s. daily max)
plot(valid_2018_2022$CI/1000, valid_2018_2022$max_customer/1000,
     xlab = "IEEE CI (in thousands)",
     ylab = "EAGLE-I CI-daily_max (in thousands)",
     xlim = c(0, 4000),
     ylim = c(0, 4000),
     col = as.factor(valid_2018_2022$NERC),
     main = "CI comparison for all NERC regions",
     cex.main = 2,
     cex.lab = 1.5)
abline(a = 0, b = 1,
       col = "red",        # Set line color (e.g., red)
       lty = 2,            # Set line type (e.g., 2 for dashed)
       lwd = 1.5)          # Set line width (optional)
legend("topright", legend = unique(as.factor(valid_2018_2022$NERC)), 
       col = unique(as.factor(valid_2018_2022$NERC)), pch = 1, title = "NERC Regions")
boxplot(log(valid_2018_2022$CI), log(valid_2018_2022$CI_moving), 
        log(valid_2018_2022$max_customer), log(valid_2018_2022$daily_ci), 
        log(valid_2018_2022$daily_ci_3day_avg),
        names = c("IEEE", "IEEE_moving_avg", "EAGLE-I_daily_max", "EAGLE-I_sum_positive_diff", 
                  "EAGLE-I_Moving_avg"), 
        # ylim = c(0, 16),
        ylab = "Log (CI)", 
        las = 1)

# (IEEE CI v.s sum_positive_diff)
plot(valid_2018_2022$CI/1000, valid_2018_2022$daily_ci/1000,
     xlab = "IEEE CI (in thousands)",
     ylab = "EAGLE-I CI-sum_diff (in thousands)",
     xlim = c(0, 8500),
     ylim = c(0, 8500),
     col = as.factor(valid_2018_2022$NERC),
     main = "CI comparison for all NERC regions", 
     cex.main = 2, 
     cex.lab = 1.5)
abline(a = 0, b = 1,
       col = "red",        # Set line color (e.g., red)
       lty = 2,            # Set line type (e.g., 2 for dashed)
       lwd = 1.5)          # Set line width (optional)
legend("topright", legend = unique(as.factor(valid_2018_2022$NERC)), 
       col = unique(as.factor(valid_2018_2022$NERC)), pch = 1, title = "NERC Regions")

# (IEEE CI v.s moving avg)
plot(valid_2018_2022$CI/1000, valid_2018_2022$daily_ci_3day_avg/1000,
     xlab = "IEEE CI (in thousands)",
     ylab = "EAGLE-I CI-moving_avg (in thousands)",
     xlim = c(0, 7000),
     ylim = c(0, 7000),
     col = as.factor(valid_2018_2022$NERC),
     main = "CI comparison for all NERC regions", 
     cex.main = 2, 
     cex.lab = 1.5)
abline(a = 0, b = 1,
       col = "red",        # Set line color (e.g., red)
       lty = 2,            # Set line type (e.g., 2 for dashed)
       lwd = 1.5)          # Set line width (optional)
legend("topright", legend = unique(as.factor(valid_2018_2022$NERC)), 
       col = unique(as.factor(valid_2018_2022$NERC)), pch = 1, title = "NERC Regions")

# (IEEE CI moving v.s moving avg)
plot(valid_2018_2022$CI_moving/1000, valid_2018_2022$daily_ci_3day_avg/1000,
     xlab = "IEEE CI-moving_avg (in thousands)",
     ylab = "EAGLE-I CI-moving_avg (in thousands)",
     xlim = c(0, 7000),
     ylim = c(0, 7000),
     col = as.factor(valid_2018_2022$NERC),
     main = "CI comparison for all NERC regions")
abline(a = 0, b = 1,
       col = "red",        # Set line color (e.g., red)
       lty = 2,            # Set line type (e.g., 2 for dashed)
       lwd = 1.5)          # Set line width (optional)
legend("topright", legend = unique(as.factor(valid_2018_2022$NERC)), 
       col = unique(as.factor(valid_2018_2022$NERC)), pch = 1, title = "NERC Regions")

# (CMI comparisons)
plot(valid_2018_2022$CMI/1000000, valid_2018_2022$customer_minutes/1000000, 
     xlab = "IEEE CMI (in millions)", 
     ylab = "EAGLE-I CMI (in millions)", 
     xlim = c(0, 5000),
     ylim = c(0, 5000),
     col = as.factor(valid_2018_2022$NERC),
     main = "CMI comparison for all NERC regions", 
     cex.main = 2, 
     cex.lab = 1.5)
abline(a = 0, b = 1,
       col = "red",        # Set line color (e.g., red)
       lty = 2,            # Set line type (e.g., 2 for dashed)
       lwd = 1.5)          # Set line width (optional)
legend("topright", legend = unique(as.factor(valid_2018_2022$NERC)), 
       col = unique(as.factor(valid_2018_2022$NERC)), pch = 1, title = "NERC Regions")

boxplot(log(valid_2018_2022$CMI), log(valid_2018_2022$customer_minutes), 
        names = c("IEEE", "EAGLE-I"), 
        ylab = "Log (CMI)", 
        las = 1)





## Comparison for each NERC region
nerc <- "TRE"
target <- valid_2018_2022[valid_2018_2022$NERC == nerc, ]


# (CI v.s daily max)
cor(target$CI, target$max_customer)
cor(target$CI, target$max_customer, method= "spearman")


plot(target$CI/1000, target$max_customer/1000, 
     xlab = "IEEE CI (in thousands)", 
     ylab = "Daily max (in thousands)", 
     xlim = c(0, 1200),
     ylim = c(0, 1200),
     col = valid_2018_2022$year,
     main = nerc)

abline(a = 0, b = 1,
       col = "red",        # Set line color (e.g., red)
       lty = 2,            # Set line type (e.g., 2 for dashed)
       lwd = 1.5)          # Set line width (optional)

legend("topright", legend = unique(valid_2018_2022$year), 
       col = unique(valid_2018_2022$year), pch = 1, title = "Years")

boxplot(log(target$CI), log(target$max_customer), 
        names = c("IEEE", "EAGLE-I"), 
        ylab = "Log (CI)", 
        las = 1)

# (CI v.s. sum positive diff)
cor(target$CI, target$daily_ci)
cor(target$CI, target$daily_ci, method = "spearman")

plot(target$CI/1000, target$daily_ci/1000, 
     xlab = "IEEE CI (in thousands)", 
     ylab = "Sum_positive_diff (in thousands)", 
     # xlim = c(0, 1200),
     # ylim = c(0, 1200),
     col = valid_2018_2022$year,
     main = nerc)

abline(a = 0, b = 1,
       col = "red",        # Set line color (e.g., red)
       lty = 2,            # Set line type (e.g., 2 for dashed)
       lwd = 1.5)          # Set line width (optional)

legend("topright", legend = unique(valid_2018_2022$year), 
       col = unique(valid_2018_2022$year), pch = 1, title = "Years")

boxplot(log(target$CI), log(target$daily_ci), 
        names = c("IEEE", "EAGLE-I"), 
        ylab = "Log (CI)", 
        las = 1)

# (CI v.s. moving avg)
cor(target$CI, target$daily_ci_3day_avg)
cor(target$CI, target$daily_ci_3day_avg, method = "spearman")

plot(target$CI/1000, target$daily_ci_3day_avg/1000, 
     xlab = "IEEE CI (in thousands)", 
     ylab = "Moving avg (in thousands)", 
     # xlim = c(0, 750),
     # ylim = c(0, 750),
     col = valid_2018_2022$year,
     main = nerc)

abline(a = 0, b = 1,
       col = "red",        # Set line color (e.g., red)
       lty = 2,            # Set line type (e.g., 2 for dashed)
       lwd = 1.5)          # Set line width (optional)

legend("topright", legend = unique(valid_2018_2022$year), 
       col = unique(valid_2018_2022$year), pch = 1, title = "Years")

boxplot(log(target$CI), log(target$daily_ci_3day_avg), 
        names = c("IEEE", "EAGLE-I"), 
        ylab = "Log (CI)", 
        las = 1)

# (IEE CI moving v.s. EAGLEI-I moving avg)
cor(target$CI_moving, target$daily_ci_3day_avg)
cor(target$CI_moving, target$daily_ci_3day_avg, method = "spearman")

plot(target$CI_moving/1000, target$daily_ci_3day_avg/1000, 
     xlab = "IEEE CI_moving (in thousands)", 
     ylab = "Moving avg (in thousands)", 
     # xlim = c(0, 550),
     # ylim = c(0, 550),
     col = valid_2018_2022$year,
     main = nerc)

abline(a = 0, b = 1,
       col = "red",        # Set line color (e.g., red)
       lty = 2,            # Set line type (e.g., 2 for dashed)
       lwd = 1.5)          # Set line width (optional)

legend("topleft", legend = unique(valid_2018_2022$year), 
       col = unique(valid_2018_2022$year), pch = 1, title = "Years")

boxplot(log(target$CI), log(target$daily_ci_3day_avg), 
        names = c("IEEE", "EAGLE-I"), 
        ylab = "Log (CI)", 
        las = 1)

# Calculate correlation coefficient and generate scatterplot for each NERC region for CMI
target <- valid_2018_2022[valid_2018_2022$NERC == "TRE", ]
cor(target$CMI, target$customer_minutes)
plot(target$CMI/1000000, target$customer_minutes/1000000, 
     xlab = "IEEE CMI (in millions)", 
     ylab = "EAGLE-I CMI (in millions)", 
     # xlim = c(0, 550),
     # ylim = c(0, 550),
     col = valid_2018_2022$year,
     main = "TRE")

abline(a = 0, b = 1,
       col = "red",        # Set line color (e.g., red)
       lty = 2,            # Set line type (e.g., 2 for dashed)
       lwd = 1.5)          # Set line width (optional)

legend("topright", legend = unique(valid_2018_2022$year), 
       col = unique(valid_2018_2022$year), pch = 1, title = "Years")

boxplot(log(target$CMI), log(target$customer_minutes), 
        names = c("IEEE", "EAGLE-I"), 
        ylab = "Log (CMI)", 
        las = 1)
