
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ea

<!-- badges: start -->

[![R-CMD-check](https://github.com/simonmoulds/ea/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/simonmoulds/ea/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The ea package provides access to Environment Agency (EA) Hydrology API.

## Installation

You can install the development version of ea from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("simonmoulds/ea")
#> Skipping install of 'ea' from a github remote, the SHA1 (450d8068) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

## Example

Here is a basic example that shows you how to use the package to
download streamflow timeseries data from the EA Hydrology API:

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

``` r
library(lubridate)
#> 
#> Attaching package: 'lubridate'
#> The following objects are masked from 'package:base':
#> 
#>     date, intersect, setdiff, union
```

``` r
library(ggplot2)
library(ea)
```

The steps required to download historical streamflow data are outlined
below:

``` r
# Get discharge stations on the River Thames within 5km of Abingdon
abingdon_stations <- ea_station_list(
  sample_of = "River Thames", observed_property = "waterFlow",
  lat = 51.6708, long = -1.2880, dist = 5
)
guid <- abingdon_stations$stationGuid
tslist <- ea_timeseries_list(guid)

# Filter to get daily mean flow 
measure <- tslist |>
  filter(period == 86400 & valueType == "mean") |> 
  pull(notation)
min_date <- Sys.Date() %m-% months(3)
ts <- ea_timeseries_values(measure, min_date = min_date)

# Alternatively: 
ts <- ea_timeseries_values(
  station_guid = guid, observed_property = "waterFlow",
  value_type = "mean", period = 86400, min_date = min_date
)
```

Once we have obtained the data we can plot the timeseries:

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

<!-- README.md is generated from README.Rmd. Please edit that file -->
