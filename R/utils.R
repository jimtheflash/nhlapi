create_request <- function(base_uri, hdrs=NULL, bdy=NULL, ...) {

  req <- httr2::request(base_uri)

  if (!is.null(hdrs)) req <- req |> httr2::req_headers(!!!hdrs)
  if (!is.null(bdy)) req <- req |> httr2::req_body_json(!!!bdy)
  # TODO handle ellipses

  return(req)

}

default_headers <- function() {

  list(
    'Accept'='*/*',
    'Content-Type'='application/json',
    'User-Agent'='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36'
  )

}

season_start_dates_lu <- function() {

  read.csv(
    system.file('nhl_season_start_dates.csv', package='nhlapi')
    )

}

sleep_default <- function() {

  5

}
