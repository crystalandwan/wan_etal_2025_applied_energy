# *****************************************************************************************
# Title: CONUS-level Reliability Metrics Calculation
# Author: Heng Wan
# Date: 2025-11-27
# Purpose: This script calculates CONUS-level System Average Interruption Frequency Index (SAIFI),
#          System Average Interruption Duration Index (SAIDI), and Customer Average Interruption
#          Duration Index (CAIDI) using both EAGLE-I and IEEE outage data. It processes
#          granular daily outage data, fills missing dates with zero outages, and applies
#          a 365-day rolling sum to calculate annual reliability metrics.
# Description:
#   1. Loads NERC-level EAGLE-I coverage and daily outage data.
#   2. Aggregates EAGLE-I data to CONUS level, interpolates missing dates (assuming zero outages),
#      and calculates 365-day rolling sums for Customer Interrupted (CI) and Customer Minutes Interrupted (CMI).
#   3. Derives SAIFI, SAIDI, and CAIDI from EAGLE-I data, using different aggregation methods
#      for CI (max method, sum method, and moving-average method)
#      and corresponding coverage metrics.
#   4. Loads IEEE customer count data and daily outage data (both Major Event Day [MED]-included and MED-excluded
#      versions, as specified in config.R).
#   5. Aggregates IEEE data to CONUS level, interpolates missing dates, and calculates 365-day
#      rolling sums for CI and CMI.
#   6. Derives SAIFI, SAIDI, and CAIDI from IEEE data.
#   7. Writes the calculated reliability metrics for both EAGLE-I and IEEE to CSV files.
# Requirements:
#   - R packages: `data.table`, `dplyr`, `zoo`, `lubridate`, `here`.
#   - A `config.R` file located in `scripts/Outage_Analysis/` relative to `here::here()`,
#     defining paths, years, and specific FIPS codes for time zone handling.
#   - Processed NERC-level EAGLE-I daily data (`eaglei_nerc_time_adjusted.csv`).
#   - Processed NERC-level EAGLE-I annual coverage data (`eaglei_coverage_by_NERC.csv`).
#   - IEEE customer counts (`customer counts.csv`).
#   - IEEE combined outage data (MED-included and MED-excluded versions, i.e., `combined.csv`, `combined excl med.csv`).
# ******************************************************************************************

# Load Required Libraries ----
library(data.table) 
library(dplyr)      
library(zoo)        
library(lubridate)  
library(here)       

# Source Configuration File ----
config_path <- here::here("scripts", "Outage_analysis", "config.R")
if (!file.exists(config_path)) {
  stop("The configuration file does not exist. Ensure the correct path:", config_path)
}
source(config_path)

# Validate all essential input paths defined in config.R---

# Validate eaglei_coverage_nerc_path
if (!file.exists(eaglei_coverage_nerc_path)) {
  stop(paste("Error: EAGLE-I NERC-level coverage data file not found at:", eaglei_coverage_nerc_path))
}
# Validate eaglei_nerc_time_adjusted_path
if (!file.exists(eaglei_nerc_time_adjusted_path)) {
  stop(paste("Error: Processed EAGLE-I NERC-level daily data file not found at:", eaglei_nerc_time_adjusted_path))
}
# Validate ieee_customers_path (used in both IEEE processing calls)
if (!file.exists(ieee_customers_path)) {
  stop(paste("Error: IEEE customer counts data file not found at:", ieee_customers_path))
}
# Validate paths for IEEE outage data based on ieee_versions_to_process
if (exists("ieee_versions_to_process") && is.list(ieee_versions_to_process)) {
  for (version_info in ieee_versions_to_process) {
    if (!file.exists(version_info$path)) {
      stop(paste("Error: IEEE outage data file (version:", version_info$output_suffix, ") not found at:", version_info$path))
    }
  }
} else {
  stop("Error: 'ieee_versions_to_process' not defined or not a list in config.R.")
}

# --- Utility Functions ---

#' Calculate 365-day rolling sums for specified columns in a data frame.
#'
#' This function takes a data frame, a list of column names, and applies
#' a 365-day centered rolling sum to each specified column.
#'
#' @param df A data frame with a 'Date' column and numeric columns for rolling sums.
#' @param columns_to_roll Character vector of column names to apply rolling sum to.
#' @param window_width Numeric. The width of the rolling window (default: 365 days).
#' @param align_type Character. Alignment of the window ("center", "left", "right").
#' @return The input data frame with new columns named `rolling_<column_name>`.
calculate_rolling_sums <- function(df, columns_to_roll, window_width = 365, align_type = "center") {
  for (col_name in columns_to_roll) {
    new_col_name <- paste0("rolling_", col_name)
    df[[new_col_name]] <- rollapply(
      df[[col_name]],
      width = window_width,
      FUN = sum,
      na.rm = TRUE, # Important if you expect NAs to be treated as 0s in sum
      align = align_type,
      fill = NA
    )
  }
  return(df)
}

# --- Main Processing Functions for EAGLE-I and IEEE ---

#' Processes EAGLE-I data to calculate CONUS-level reliability metrics.
#'
#' Loads EAGLE-I coverage and daily outage data, aggregates to CONUS-level,
#' interpolates missing dates, calculates rolling sums, and derives SAIFI,
#' SAIDI, and CAIDI metrics.
#'
#' @param coverage_path Path to the EAGLE-I NERC-level annual coverage CSV.
#' @param nerc_daily_path Path to the NERC-level daily EAGLE-I outage data CSV.
#' @param start_year Numeric. The starting year for the analysis.
#' @param end_year Numeric. The ending year for the analysis.
#' @param output_filename Path where the final reliability metrics CSV will be saved.
#' @return A data.table containing the calculated EAGLE-I reliability metrics.
process_eaglei_reliability <- function(coverage_path, nerc_daily_path, start_year, end_year, output_filename) {
  message(paste("\nCalculating EAGLE-I CONUS-level reliability metrics for", start_year, "-", end_year))
  
  # Read in NERC-level annual coverage
  coverage_nerc <- fread(coverage_path)
  # Aggregate coverage to CONUS
  coverage_conus <- coverage_nerc %>%
    group_by(year) %>%
    summarise(total_min_covered = sum(min_covered, na.rm = TRUE),
              total_max_covered = sum(max_covered, na.rm = TRUE),
              .groups = 'drop')
  
  # Read in the NERC-level daily EAGLE-I dataset
  eaglei_daily_nerc <- fread(nerc_daily_path) %>%
    mutate(year = year(as.Date(Date))) # Add year column
  
  # Filter to analysis years
  eaglei_daily_nerc <- eaglei_daily_nerc[
    eaglei_daily_nerc$year >= start_year & eaglei_daily_nerc$year <= end_year,
  ]
  
  # Aggregate eaglei by Date to CONUS-level
  eaglei_conus <- eaglei_daily_nerc[, .(
    max_customer = sum(max_customer, na.rm = TRUE),
    daily_ci = sum(daily_ci, na.rm = TRUE),
    daily_ci_3day_avg = sum(daily_ci_3day_avg, na.rm = TRUE),
    customer_minutes = sum(customer_minutes, na.rm = TRUE)
  ), by = .(Date)] %>%
    mutate(year = year(as.Date(Date)))
  
  # Merge coverage with eaglei daily data by year
  eaglei_conus <- merge(eaglei_conus, coverage_conus, by = "year", all.x = TRUE)
  
  # Generate a complete date sequence for the analysis period
  full_date_sequence <- as.IDate(seq(as.Date(paste0(start_year, "-01-01")), as.Date(paste0(end_year, "-12-31")), by = "day"))
  
  # Create a complete data frame to account for missing dates (dates with no outage)
  eaglei_conus_complete <- data.frame(Date = full_date_sequence) %>%
    left_join(eaglei_conus, by = "Date") %>%
    # Fill in missing values (assume zero CI/CMI for missing dates)
    mutate(
      max_customer = ifelse(is.na(max_customer), 0, max_customer),
      daily_ci = ifelse(is.na(daily_ci), 0, daily_ci),
      daily_ci_3day_avg = ifelse(is.na(daily_ci_3day_avg), 0, daily_ci_3day_avg),
      customer_minutes = ifelse(is.na(customer_minutes), 0, customer_minutes)
      # annual coverage numbers (total_min_covered, total_max_covered) will be filled by year
    ) %>%
    # Fill `year` and `total_min_covered`/`total_max_covered` by year
    # This assumes that if an entire year is missing for coverage (which it shouldn't be if merged correctly),
    # it might remain NA. A simpler approach if coverage is guaranteed per year:
    group_by(year) %>%
    tidyr::fill(total_min_covered, total_max_covered, .direction = "downup") %>%
    ungroup() %>%
    # For dates where year is NA (e.g. if the date sequence extends beyond available data), remove them
    filter(!is.na(year)) %>%
    arrange(Date) # Ensure data is ordered by date for rolling sums
  
  # Calculate rolling sums for each CI/CMI column
  eaglei_conus_complete <- calculate_rolling_sums(
    eaglei_conus_complete,
    columns_to_roll = c("max_customer", "daily_ci", "daily_ci_3day_avg", "customer_minutes")
  )
  
  # Calculate the reliability metrics
  eaglei_conus_complete <- eaglei_conus_complete %>%
    mutate(
      # SAIFI = (Total Annual Customer Interruptions) / (Total Number of Customers Served)
      saifi_max_method_min_coverage = rolling_max_customer / total_min_covered,
      saifi_max_method_max_coverage = rolling_max_customer / total_max_covered,
      saifi_sum_method_min_coverage = rolling_daily_ci / total_min_covered,
      saifi_sum_method_max_coverage = rolling_daily_ci / total_max_covered,
      saifi_moving_avg_min_coverage = rolling_daily_ci_3day_avg / total_min_covered,
      saifi_moving_avg_max_coverage = rolling_daily_ci_3day_avg / total_max_covered,
      
      # SAIDI = (Total Annual Customer Interruption Durations) / (Total Number of Customers Served)
      saidi_min_coverage = rolling_customer_minutes / total_min_covered,
      saidi_max_coverage = rolling_customer_minutes / total_max_covered, # Note: using max covered in denominator here as in original script
      
      # CAIDI = (Total Annual Customer Interruption Durations) / (Total Annual Customer Interruptions)
      caidi_max_method = rolling_customer_minutes / rolling_max_customer,
      caidi_sum_method = rolling_customer_minutes / rolling_daily_ci,
      caidi_moving_avg_method = rolling_customer_minutes / rolling_daily_ci_3day_avg
    )
  
  # Filter the result to exclude the partial rolling window periods (first/last 182 days)
  # This ensures that only valid center-aligned 365-day rolling sums are included.
  # The 'window_width' / 2 determines the number of days to filter from each end.
  days_to_exclude_each_end <- floor(365 / 2)
  eaglei_final <- eaglei_conus_complete %>%
    filter(
      Date >= (as.Date(paste0(start_year, "-01-01")) + days_to_exclude_each_end) &
        Date <= (as.Date(paste0(end_year, "-12-31")) - days_to_exclude_each_end)
    )
  
  # Write out the result
  # Ensure directory exists
  output_dir <- dirname(output_filename)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  fwrite(eaglei_final, output_filename)
  message(paste("EAGLE-I CONUS reliability metrics saved to:", output_filename))
  
  return(eaglei_final)
}

#' Processes IEEE data to calculate CONUS-level reliability metrics.
#'
#' Loads IEEE customer count and daily outage data, aggregates to CONUS-level,
#' interpolates missing dates, calculates rolling sums, and derives SAIFI,
#' SAIDI, and CAIDI metrics.
#'
#' @param ieee_outages_path Path to the IEEE daily outage data CSV (e.g., combined.csv).
#' @param ieee_customer_counts_path Path to the IEEE annual customer counts CSV.
#' @param start_year Numeric. The starting year for the analysis.
#' @param end_year Numeric. The ending year for the analysis.
#' @param output_filename Path where the final reliability metrics CSV will be saved.
#' @return A data.table containing the calculated IEEE reliability metrics.
process_ieee_reliability <- function(ieee_outages_path, ieee_customer_counts_path, start_year, end_year, output_filename) {
  message(paste("\nCalculating IEEE CONUS-level reliability metrics for", start_year, "-", end_year))
  
  # Read in IEEE customer coverage data
  ieee_coverage <- fread(ieee_customer_counts_path)
  # Aggregate coverage to CONUS
  ieee_coverage_conus <- ieee_coverage %>%
    group_by(Year) %>%
    summarise(total_customers = sum(Customers, na.rm = TRUE), .groups = 'drop')
  
  # Read in IEEE daily outage data
  ieee_combined <- fread(ieee_outages_path) %>%
    mutate(Year = year(as.Date(Date))) # Add Year column
  
  # Filter to analysis years
  ieee_combined <- ieee_combined[ieee_combined$Year >= start_year & ieee_combined$Year <= end_year, ]
  
  # Aggregate IEEE by Date to CONUS-level
  ieee_conus <- ieee_combined[, .(
    CI = sum(CI, na.rm = TRUE),
    CMI = sum(CMI, na.rm = TRUE)
  ), by = .(Date)] %>%
    mutate(Year = year(as.Date(Date)))
  
  # Merge coverage with IEEE daily data by year
  ieee_conus <- merge(ieee_conus, ieee_coverage_conus, by = "Year", all.x = TRUE)
  
  # Generate a complete date sequence for the analysis period
  full_date_sequence <- as.IDate(seq(as.Date(paste0(start_year, "-01-01")), as.Date(paste0(end_year, "-12-31")), by = "day"))
  
  # Create a complete data frame to account for missing dates (dates with no outage)
  ieee_conus_complete <- data.frame(Date = full_date_sequence) %>%
    left_join(ieee_conus, by = "Date") %>%
    # Fill in missing values (assume zero CI/CMI for missing dates)
    mutate(
      CI = ifelse(is.na(CI), 0, CI),
      CMI = ifelse(is.na(CMI), 0, CMI)
    ) %>%
    # Fill `Year` and `total_customers` by year
    group_by(Year) %>%
    tidyr::fill(total_customers, .direction = "downup") %>%
    ungroup() %>%
    # Filter out dates if year is NA (e.g. outside analysis range)
    filter(!is.na(Year)) %>%
    arrange(Date) # Ensure data is ordered by date for rolling sums
  
  # Calculate rolling sums for CI and CMI
  ieee_conus_complete <- calculate_rolling_sums(
    ieee_conus_complete,
    columns_to_roll = c("CI", "CMI")
  )
  
  # Calculate the IEEE reliability metrics
  ieee_conus_complete <- ieee_conus_complete %>%
    mutate(
      SAIFI = rolling_CI / total_customers,
      CAIDI = rolling_CMI / rolling_CI,
      SAIDI = rolling_CMI / total_customers
    )
  
  # Filter the result to exclude the partial rolling window periods (first/last 182 days)
  days_to_exclude_each_end <- floor(365 / 2)
  ieee_final <- ieee_conus_complete %>%
    filter(
      Date >= (as.Date(paste0(start_year, "-01-01")) + days_to_exclude_each_end) &
        Date <= (as.Date(paste0(end_year, "-12-31")) - days_to_exclude_each_end)
    )
  
  # Write out the result
  # Ensure directory exists
  output_dir <- dirname(output_filename)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  fwrite(ieee_final, output_filename)
  message(paste("IEEE CONUS reliability metrics saved to:", output_filename))
  
  return(ieee_final)
}

# --- Main Script Execution ---
message("Starting CONUS-level reliability metrics calculation.")

# 1. Process EAGLE-I Dataset ----
eaglei_reliability_results <- process_eaglei_reliability(
  coverage_path = eaglei_coverage_nerc_path,
  nerc_daily_path = eaglei_nerc_time_adjusted_path,
  start_year = 2018,
  end_year = 2022,
  output_filename = eaglei_reliability_path
)

# 2. Process IEEE Datasets (MED-included and MED-excluded) ----
# Loop through the IEEE versions defined in config.R
for (version_info in ieee_versions_to_process) {
  # Construct the full path to the outage data for the current version
  # `get()` is used here to retrieve the variable value whose name is stored in `version_info$path`
  current_ieee_outages_path <- version_info$path
  
  # Construct the output filename using the base path and the version-specific suffix
  current_output_filename <- paste0(
    tools::file_path_sans_ext(here("data", "Output")),
    "/IEEE_rolling_reliability",
    version_info$output_suffix,
    ".csv"
  )
  
  ieee_reliability_results <- process_ieee_reliability(
    ieee_outages_path = current_ieee_outages_path,
    ieee_customer_counts_path = ieee_customers_path,
    start_year = 2003,
    end_year = 2023,
    output_filename = current_output_filename
  )
}

message("\nAll CONUS-level reliability metrics calculations complete.")