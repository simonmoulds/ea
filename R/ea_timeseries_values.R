#' Get timeseries data for one or more stations
#'
#' @description
#' `ea_timeseries_values()` is used to retrieve timeseries data in 
#' one of two ways. Firstly, you may supply the `measure` argument, 
#' which is a string that uniquely identifies the measurement and 
#' is provided in the `notation` column in the tibble returned by 
#' [ea_timeseries_list()]. Alternatively, you can provide `station_guid`, 
#' `observed_property`, `value_type` and `period` to uniquely define 
#' the measurement.
#' 
#' @param measure Character. String that uniquely identifies the 
#'   measurement. This is provided in the `notation` column of the
#'   tibble returned by [ea_timeseries_list()].
#' @param station_guid Character. Station global unique identifier. 
#'   Not used if `measure` is supplied.
#' @param observed_property Character. Observation property. Not 
#'   used if `measure` is supplied.
#' @param value_type Character. Value type. Not used if `measure` 
#'   is supplied.
#' @param period Integer. Period between readings in seconds. Not
#'   used if `measure` is supplied.
#' @param date Date or character formatted "YYYY-MM-DD"
#' @param min_date Date or character formatted "YYYY-MM-DD"
#' @param max_date Date or character formatted "YYYY-MM-DD"
#' @param earliest Logical. If TRUE, will return the earliest
#'   measurements on record.
#' @param latest Logical. If TRUE, will return the latest
#'   measurements on record.
#' @param ... Additional arguments. None implemented.
#'
#' @return A tibble.
#' @export
#' @examples 
#' \dontrun{
#' 
#' # Get discharge stations on the River Thames within 5km of Abingdon
#' abingdon_stations <- ea_station_list(
#'   sample_of = "River Thames", observed_property = "waterFlow",
#'   lat = 51.6708, long = -1.2880, dist = 5
#' )
#' guid <- abingdon_stations$stationGuid
#' tslist <- ea_timeseries_list(guid)
#' 
#' # Filter to get daily mean flow 
#' measure <- tslist |> 
#'   filter(period == 86400 & valueType == "mean") |> 
#'   pull(notation)
#' min_date <- Sys.Date() - lubridate::days(31)
#' ts <- ea_timeseries_values(measure, min_date = min_date)
#' 
#' # Alternatively: 
#' ts <- ea_timeseries_values(
#'   station_guid = guid, observed_property = "waterFlow",
#'   value_type = "mean", period = 86400, min_date = min_date
#' )
#' }
ea_timeseries_values <- function(measure,
                                 station_guid,
                                 observed_property,
                                 value_type,
                                 period,
                                 date,
                                 min_date,
                                 max_date,
                                 earliest = FALSE,
                                 latest = FALSE,
                                 ...) {

  api_url <- paste0(ea_base_url(), "/data/readings.json")

  if (missing(measure)) {
    if (missing(station_guid) && missing(observed_property)) {
      stop(
        strwrap(
          "Either `measure` or `station` and `observation_type` should be 
          provided", prefix = " ", initial = ""
        )
      )
    } else {
      if (missing(period)) {
        warning("`period` not supplied: defaulting to daily (86400)")
        period <- 86400
      } else { 
        if (!is.numeric(period)) { 
          stop(
            strwrap(
              "`period` should be an integer number of seconds between 
              measurements (e.g. for daily data set period to 86400)",
              prefix = " ", initial = ""
            )
          )
        }
      }
    }
    api_query <- list(
      station = station_guid,
      observedProperty = observed_property,
      period = period
    )
    if (!missing(value_type)) {
      api_query <- c(api_query, list(valueType = value_type))
    }

  } else {
    if (!missing(station_guid) ||
        !missing(observed_property) ||
        !missing(value_type)) {

      warning(
        strwrap(
          "Arguments `station_guid`, `observed_property` and `period` 
          are redundant when `measure` is supplied", 
          prefix = " ", initial = ""
        )
      )
    }
    api_query <- list(measure = measure)
  }

  ## Handle time queries
  if (earliest || latest) {

    ## Can't both be true
    if (earliest && latest) {
      stop("Arguments `earliest` and `latest` cannot both be true")
    }

    ## latest/earliest take precedence
    if (!missing(date) || !missing(min_date) || !missing(max_date)) {
      warning(
        strwrap(
          "Arguments `date`, `min_date` and `max_date` are redundant 
          when either `earliest` or `latest` is  true",
          prefix = " ", initial = ""
        )
      )
    }

    ## Update query
    if (earliest) {
      api_query <- c(api_query, list(earliest = TRUE))
    } else {
      api_query <- c(api_query, list(latest = TRUE))
    }

  } else {
    if (!missing(date)) {
      date <- format(as.POSIXct(date), "%Y-%m-%d")
      api_query <- c(api_query, date = date)
    }
    if (!missing(min_date)) {
      min_date <- format(as.POSIXct(min_date), "%Y-%m-%d")
      api_query <- c(api_query, `min-date` = min_date)
    }
    if (!missing(max_date)) {
      max_date <- format(as.POSIXct(max_date), "%Y-%m-%d")
      api_query <- c(api_query, `max-date` = max_date)
    }
  }

  ## Set limit to hard maximum
  api_query <- c(api_query, list(`_limit` = "2000000"))

  ## Send request
  raw <- request(api_url) |>
    req_url_query(!!!api_query, .multi = "explode") |> 
    req_timeout(30) |>
    req_user_agent("sepa (https://github.com/simonmoulds/ea)") |>
    req_perform()

  # Converting raw content to str and then parsing JSON works better
  # than using `resp_body_json`
  raw_content <- resp_body_string(raw)

  ## Parse text
  json_content <- jsonlite::fromJSON(raw_content)

  ## Convert to tibble
  content_dat <- tibble::as_tibble(
    x = json_content$items,
    .name_repair = "minimal"
  )
  content_dat <- content_dat |>
    tidyr::unnest(cols = "measure") |>
    rename(measure = 1)

  root <- paste0(ea_base_url(), "/id/measures/")
  content_dat <- content_dat
  content_dat$measure <- gsub(root, "", content_dat$measure)
  content_dat$date <- as.Date(content_dat$date)
  # TODO check timezone is correct
  content_dat$dateTime <- as.POSIXct(content_dat$dateTime, tz = "GMT")
  return(content_dat)
}