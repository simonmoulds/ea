% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ea_station_list.R
\name{ea_station_list}
\alias{ea_station_list}
\title{Get station metadata}
\usage{
ea_station_list(
  station_guid,
  sample_of,
  status_label,
  observed_property,
  open_from,
  open_to,
  long,
  lat,
  easting,
  northing,
  dist,
  ...
)
}
\arguments{
\item{station_guid}{Character. Station globally unique identifer. Only
one station can be provided at once.}

\item{sample_of}{Character. River name (e.g. "River Thames"). Note
that rivers are named inconsistently, so it may be necessary to
supply multiple versions of river names.}

\item{status_label}{Character. One of 'Active', 'Suspended' or 'Closed'}

\item{observed_property}{Character.}

\item{open_from}{Date or character formatted "YYYY-MM-DD"}

\item{open_to}{Date or character formatted "YYYY-MM-DD"}

\item{long}{Numeric. The longitude of the centre of the search area
with radius \code{dist}.}

\item{lat}{Numeric. The latitude of the centre of the search area
with radius \code{dist}.}

\item{easting}{Numeric. The easting of the centre of the search area
with radius \code{dist}.}

\item{northing}{Numeric. The northing of the centre of the search area
with radius \code{dist}.}

\item{dist}{Numeric. Distance in kilometres from the point
specified by \code{long} and \code{lat} or \code{easting} and \code{northing}.}

\item{...}{Additional arguments. None implemented.}
}
\value{
A tibble.
}
\description{
Returns metadata about measurement stations in the EA network.
}
\section{Measured variables}{


The argument \code{observed_property} allows you to select stations based on
the variables they measure. Here are the available options:
\itemize{
\item \code{waterFlow}
\item \code{waterLevel}
\item \code{rainfall}
\item \code{groundwaterLevel}
\item \code{dissolved-oxygen}
\item \code{fdom}
\item \code{bga}
\item \code{turbidity}
\item \code{chlorophyll}
\item \code{conductivity}
\item \code{temperature}
\item \code{ammonium}
\item \code{nitrate}
\item \code{ph}
}
}

\examples{
\dontrun{ 
# Get all stations that collect river discharge 
x <- ea_station_list(observed_property = "waterFlow")

# Get metadata for a certain station
id <- x$stationGuid[1]
y <- ea_station_list(station_guid = id)

# Get metadata for all stations on the Thames
thames_nms <- c("River Thames", "RIVER THAMES", "THAMES")
thames_stations <- ea_station_list(sample_of = thames_nms)

# Get discharge stations on the River Thames within 20km of Abingdon
abingdon_stations <- ea_station_list(
  sample_of = thames_nms, observed_property = "waterFlow",
  lat = 51.6708, long = -1.2880, dist = 20
)
}
}
