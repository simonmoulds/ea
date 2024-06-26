

ea_base_url <- function() {
  root <- "http://environment.data.gov.uk/hydrology"
  return(root)
}


#' Get tibble with station information
#'
#' Returns metadata about stations in the EA network.
#'
#' @param station_id Character.
#' @param sample_of Character.
#' @param status_label Character. One of 'Active', 'Suspended' or
#' @param observed_property Character.
#' @param open_from Date or character formatted "YYYY-MM-DD"
#' @param open_to Date or character formatted "YYYY-MM-DD"
#' @param long Numeric. Longitude.
#' @param lat Numeric. Latitude.
#' @param easting Numeric. Easting.
#' @param northing Numeric. Northing.
#' @param dist Numeric. Distance in kilometres from the point
#'   specified by `long` and `lat` or `easting` and `northing`.
#' @param ... Additional arguments. None implemented.
#'
#' @return A tibble.
#' @export
ea_station_list <- function(station_id,
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
                            ...) {

  api_url <- ea_base_url()
  api_url <- paste0(api_url, "/id/stations")

  api_query <- list()

  if (!missing(station_id)) {
    api_url <- paste0(api_url, "/", station_id)

  } else {

    if (!missing(sample_of)) {
      api_query <- c(api_query, list(riverName = sample_of))
    }

    if (!missing(status_label)) {
      valid_status <- c("Active", "Suspended", "Closed")
      if (!status_label %in% valid_status) {
        stop(
          strwrap(
            "Argument `status_label` must be one of Active, Suspended 
            or Closed", prefix = " ", initial = ""
          )
        )
      }
      api_query <- c(api_query, list(`status.label` = status_label))
    }

    if (!missing(observed_property)) {
      valid_properties <- c(
        "waterFlow", "waterLevel", "rainfall", "groundwaterLevel",
        "dissolved-oxygen", "fdom", "bga", "turbidity", "chlorophyll",
        "conductivity", "temperature", "ammonium", "nitrate", "ph"
      )
      if (!observed_property %in% valid_properties) {
        stop("Invalid observed property")
      }
      ## TODO more than one observed property allowed?
      api_query <- c(api_query, list(observedProperty = observed_property))
    }

    ## This endpoint seems to be unreliable
    if (!missing(open_from) | !missing(open_to)) {
      api_url <- paste0(ea_base_url(), "/id/open/stations")
      if (missing(open_to)) {
        warning(
          strwrap(
            "Argument `open_to` is required if `open_from` is provided: 
            setting `open_to` to today's date", prefix = " ", initial = ""
          )
        )
        open_to <- Sys.Date()
      }
      if (missing(open_from)) {
        warning(
          strwrap(
            "Argument `open_from` is required if `open_to` is provided: 
            setting `open_from` to 1900-01-01", prefix = " ", initial = ""
          )
        )
        open_from <- Sys.Date()
      }
      api_query <- c(api_query, list(from = open_from, to = open_to))
    }

    ## Spatial query
    if (!missing(lat) | !missing(long)) {
      if (missing(lat) | missing(long)) {
        stop("Both `lat` and `long` are required")
      }
      if (missing(dist)) {
        warning(
          strwrap(
            "Argument `dist` is required if `lat` and `long` are provided: 
            setting `dist` to 5km", prefix = " ", initial = ""
          )
        )
        dist <- 5
      }
      api_query <- c(api_query, list(lat = lat, long = long, dist = dist))
    }

    if (!missing(easting) | !missing(northing)) {

      if (!missing(lat) | !missing(long)) {
        warning(
          strwrap(
            "Arguments `lat` and `long` were also provided: ignoring `easting` 
            and `northing`", prefix = " ", initial = ""
          )
        )
      } else {

        if (missing(easting) | missing(northing)) {
          stop("Both `easting` and `northing` are required")
        }
        if (missing(dist)) {
          warning(
            strwrap(
              "Argument `dist` is required if `easting` and `northing` are 
              provided: setting `dist` to 5km", prefix = " ", initial = ""
            )
          )
          dist <- 5
        }
        api_query <- c(
          api_query, list(northing = northing, easting = easting, dist = dist)
        )
      }
    }
  }

  ## Increase default limit
  api_query <- c(api_query, list(`_limit` = "2000000")) #, `_view` = "minimal"))

  ## Send request
  raw <- request(api_url) |> 
    req_url_query(!!!api_query) |> 
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

  return(content_dat)
}


#' Get list of available time series for one or more stations
#'
#' @param station_id Character or numeric. Either a single station ID or a
#'   vector of station IDs. This corresponds to the `notation` column in
#'   metadata returned by `ea_station_list`.
#' @param ... Additional arguments. None implemented.
#'
#' @return A tibble.
#' @export
ea_timeseries_list <- function(station_id, ...) {

  api_url <- ea_base_url()
  api_url <- paste0(api_url, "/id/measures")
  
  api_query <- list(station = station_id)

  ## Send request
  raw <- request(api_url) |> 
    req_url_query(!!!api_query, .multi = "explode") |> 
    req_timeout(30) |>
    req_user_agent("sepa (https://github.com/simonmoulds/ea)") |>
    req_perform()

  raw_content <- resp_body_string(raw)

  ## Parse text
  json_content <- jsonlite::fromJSON(raw_content)

  ## Convert to tibble
  content_dat <- tibble::as_tibble(
    x = json_content$items,
    .name_repair = "minimal"
  )

  return(content_dat)
}


#' Get time series data for one or more stations
#'
#' @param measure Character.
#' @param station_id Character. Station GUID.
#' @param observed_property Character. Observation property.
#' @param value_type Character. Value type.
#' @param period Integer. Period between readings in seconds. 
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
ea_timeseries_values <- function(measure,
                                 station_id,
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
    if (missing(station_id) & missing(observed_property)) {
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
      }
    }
    api_query <- list(
      station = station_id,
      observedProperty = observed_property,
      period = period
    )
    if (!missing(value_type)) {
      api_query <- c(api_query, list(valueType = value_type))
    }

  } else {
    if (!missing(station_id) ||
        !missing(observed_property) ||
        !missing(value_type)) {

      warning(
        strwrap(
          "Arguments `station_id`, `observed_property` and `period` 
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
  content_dat <- content_dat |> 
    mutate(measure = gsub(root, "", measure)) |> 
    mutate(date = as.Date(date), dateTime = as.POSIXct(dateTime))
  return(content_dat)
}
