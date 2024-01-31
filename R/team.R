team_abbrevs <- function() {



}


get_team_roster_seasons <- function(team_abbrev, season) {

  trs_req <- create_request(
    base_uri=paste0('https://api-web.nhle.com/v1/roster/', team_abbrev, '/', season),
    hdrs=default_headers()
  )

  trs_json <- httr2::req_perform(trs_req) |>
    httr2::resp_body_json()

  return(trs_json)

}

get_team_roster <- function(team_abbrev, season) {



}
