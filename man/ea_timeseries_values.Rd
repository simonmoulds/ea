% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ea_station_list.R
\name{ea_timeseries_values}
\alias{ea_timeseries_values}
\title{Get time series data for one or more stations}
\usage{
ea_timeseries_values(
  measure,
  station_id,
  observed_property,
  value_type,
  period,
  date,
  min_date,
  max_date,
  earliest = FALSE,
  latest = FALSE,
  ...
)
}
\arguments{
\item{measure}{Character.}

\item{station_id}{Character. Station GUID.}

\item{observed_property}{Character. Observation property.}

\item{value_type}{Character. Value type.}

\item{period}{Integer. Period between readings in seconds.}

\item{date}{Date or character formatted "YYYY-MM-DD"}

\item{min_date}{Date or character formatted "YYYY-MM-DD"}

\item{max_date}{Date or character formatted "YYYY-MM-DD"}

\item{earliest}{Logical. If TRUE, will return the earliest
measurements on record.}

\item{latest}{Logical. If TRUE, will return the latest
measurements on record.}

\item{...}{Additional arguments. None implemented.}
}
\value{
A tibble.
}
\description{
Get time series data for one or more stations
}
