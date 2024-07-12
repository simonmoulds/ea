#' Get station metadata
#'
#' Returns metadata about measurement stations in the EA network.
#' 
#' @section Measured variables:
#' 
#' The argument `observed_property` allows you to select stations based on
#' the variables they measure. Here are the available options:
#' * `waterFlow`
#' * `waterLevel`
#' * `rainfall`
#' * `groundwaterLevel`
#' * `dissolved-oxygen`
#' * `fdom`
#' * `bga`
#' * `turbidity`
#' * `chlorophyll`
#' * `conductivity`
#' * `temperature`
#' * `ammonium`
#' * `nitrate`
#' * `ph`
#'
#' @param station_guid Character. Station globally unique identifer. Only 
#'   one station can be provided at once.  
#' @param sample_of Character. River name (e.g. "River Thames"). Note
#'   that rivers are named inconsistently, so it may be necessary to 
#'   supply multiple versions of river names.
#' @param status_label Character. One of 'Active', 'Suspended' or 'Closed'
#' @param observed_property Character.
#' @param open_from Date or character formatted "YYYY-MM-DD"
#' @param open_to Date or character formatted "YYYY-MM-DD"
#' @param long Numeric. The longitude of the centre of the search area 
#'   with radius `dist`.
#' @param lat Numeric. The latitude of the centre of the search area 
#'   with radius `dist`. 
#' @param easting Numeric. The easting of the centre of the search area 
#'   with radius `dist`.
#' @param northing Numeric. The northing of the centre of the search area 
#'   with radius `dist`.
#' @param dist Numeric. Distance in kilometres from the point
#'   specified by `long` and `lat` or `easting` and `northing`.
#' @param ... Additional arguments. None implemented.
#'
#' @return A tibble.
#' @export
#' @examples
#' \dontrun{ 
#' # Get all stations that collect river discharge 
#' x <- ea_station_list(observed_property = "waterFlow")
#' 
#' # Get metadata for a certain station
#' id <- x$stationGuid[1]
#' y <- ea_station_list(station_guid = id)
#' 
#' # Get metadata for all stations on the Thames
#' thames_nms <- c("River Thames", "RIVER THAMES", "THAMES")
#' thames_stations <- ea_station_list(sample_of = thames_nms)
#' 
#' # Get discharge stations on the River Thames within 20km of Abingdon
#' abingdon_stations <- ea_station_list(
#'   sample_of = thames_nms, observed_property = "waterFlow",
#'   lat = 51.6708, long = -1.2880, dist = 20
#' )
#' }
ea_station_list <- function(station_guid,
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

  if (!missing(station_guid)) {
    if (length(station_guid) > 1) { 
      stop("Only one station GUID can be provided at once")
    }
    api_url <- paste0(api_url, "/", station_guid)
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
      if (!all(observed_property %in% valid_properties)) {
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
  return(content_dat)
}