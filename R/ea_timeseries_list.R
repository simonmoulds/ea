#' Get available timeseries for one or more stations
#'
#' @param station_guid Character. Station global unique identifier. 
#'   Either a single station ID or a vector of station IDs. This 
#'   corresponds to the `stationGuid` column in metadata returned 
#'   by `ea_station_list`.
#' @param ... Additional arguments. None implemented.
#'
#' @return A tibble.
#' @export
#' @examples
#' \dontrun{
#' # Get discharge stations on the River Thames within 20km of Abingdon
#' abingdon_stations <- ea_station_list(
#'   sample_of = "River Thames", observed_property = "waterFlow",
#'   lat = 51.6708, long = -1.2880, dist = 20
#' )
#' guid <- abingdon_stations$stationGuid
#' tslist <- ea_timeseries_list(guid)
#' }
ea_timeseries_list <- function(station_guid, ...) {

  api_url <- ea_base_url()
  api_url <- paste0(api_url, "/id/measures")
  api_query <- list(station = station_guid)

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