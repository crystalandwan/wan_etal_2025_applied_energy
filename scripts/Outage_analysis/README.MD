All the scripts within this folder are used for comparing IEEE and EAGLE-I outage datasets. 
1. "config.R" contains all input/output data paths. 
2. "Aggregate_eaglei_from_15_min_to_daily.R" contains utility functions to aggregate 15-minute EAGLE-I outage data, including customer interrupted (CI) and customer minutes interrupted (CMI), to daily resolution. 
3. "Customer_coverage_comparisons.R" is the script to compare the utility customer coverage between the two datasets. 
4. "Comparisons_between_ieee_and_eaglei.R" is the script to statistically compare the two datasets. 
5. "CONUS_level_reliability_metrics_calculation.R" is the script to calculate reliability metrics (i.e., SAIDI, SAIFI, and CAIDI) for the two outage datasets based on 365-day rolling average method. 

