# wan_etal_2025_applied_energy

**Fit for Purpose Power Outage Datasets: On using EAGLE-I and IEEE for Reliability Studies **

Heng Wan<sup>1\*</sup>, Casey D. Burleyson<sup>1</sup>, Heide Caswell<sup>3</sup>, Daniel Donaldson<sup>4</sup>, Tyler Jones<sup>5</sup> and Nathalie Voisin<sup>1, 2</sup>

<sup>1 </sup>Pacific Northwest National Laboratory, Richland, WA, USA.

<sup>2 </sup>University of Washington, Seattle, WA 98109, USA.

<sup>3 </sup>Public Utility Commission of Oregon, Salem, OR 97308, USA.

<sup>4 </sup>University of Birmingham, Birmingham, United Kingdom.

<sup>5 </sup>Reliciti, San Antonio, TX, USA.

\* corresponding author:  heng.wan@pnnl.gov

## Abstract
Power outage datasets report on the spatial and temporal characteristics of customers without electricity. They have been used to understand vulnerabilities and resilience and to provide insights into trends in reliability and compliance. We compare two United States outage datasets, EAGLE-I and IEEE, from 2018-2022 to evaluate their strengths and weaknesses for different scientific questions and identify complementarities. The datasets differ in collection methodologies and in temporal and spatial scales. EAGLE-I captures a snapshot of outages sourced from the web, whereas IEEE compiles data from utilities directly. We use outage statistics, correlations of daily Customers Interrupted (CI), and case studies of extreme events to illustrate their differences. Despite processing the datasets to a common spatial and temporal resolution, direct comparisons are challenging due to differences in their methodologies and data collection approaches. EAGLE-I covers a larger share of customers, contributing to systematically higher CI and the associated estimated Customer Minutes Interrupted (CMI). Pooled across NERC reliability regions, we find weak linear mapping in CI between the two datasets, indicating that, in general, their trends in reliability metrics would not be consistent. Consistency is higher for some NERC regions, specifically Florida, Northwest, Southwest Power Pool, and Texas. EAGLE-I is advantageous for fine-scale, event-focused analysis. The IEEE data is more suitable for long-term regional benchmarking and analyses that support mapping between power outages and evolving asset characteristics. We conclude that efforts to promote coordination and harmonization between the datasets are needed to expand analytical capabilities about reliability events.

## Journal reference
_your journal reference_

## Data reference

### Input data
Brelsford, C. et al. (2024). A dataset of recorded electricity outages by United States county 2014–2022. Scientific Data 11, 271. https://doi.org/10.1038/s41597-024-03095-5
Caswell, H., & Jones, T. (2026). Anonymized IEEE Power Outage Data for the United States (1.0) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.18158530

### Output data
N.A.

## Reproduce my experiment
Clone this repository to get access to the R codes used to analyze the IEEE and EAGLE-I outage datasets. You need to download EAGLEI-I outage dataset from https://doi.ccs.ornl.gov/ui/doi/435 and IEEE outage dataset from https://doi.org/10.5281/zenodo.18158530
|Script Name | Description |
| --- | --- |
|Aggregate_eaglei_from_15min_to_daily.R | Utility functions to aggregate 15-minutes EAGLE-I outage data to daily |
|Customer_coverage_comparisons.R | Compare utility customer coverage between the two outage datasets|
|Comparisons_between_ieee_and_eaglei | Compare IEEE and EAGLE-I outage dataset based on statistical analysis|
|CONUS_level_reliability_metrics_calculation.R | Calculate reliability metrics for the two datasets at the CONUS-level|


## Reproduce my figures
Use the scripts found in the `figures` directory to reproduce the figures used in this publication.

| Figure Number(s) | Script Name | Description |
| --- | --- | --- |
| 3 | `Rolling_reliability_time_series_plot.ipynb` | Plot time-series reliability metrics for the two outage datasets|
|4, 5, 6| `Scatter_plots.ipynb` | Generate scatter plots to compare customer interrupted (CI) values between the two outage datasets|
|7|`Case_event_comparisons.ipynb` | Compare the two outage datasets using case studies|

