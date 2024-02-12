
player_shifts <- function(game_id) {

  # TODO: lookup how to parse these stupid cayenneExp params for httr2
  url <- paste0("https://api.nhle.com/stats/rest/en/shiftcharts?"
                , "cayenneExp=gameId=", game_id, "%20and%20((duration%20!=%20%2700:00%27%20and%20typeCode%20=%20517)%20or%20typeCode%20!=%20517%20)"
                , "&exclude=teamName")
  resp <- jsonlite::fromJSON(url)
  dplyr::bind_rows(resp$data)

}

full_pbp <- function(game_id) {

  pbp_html <- rvest::read_html('https://www.nhl.com/scores/htmlreports/20232024/PL020805.HTM') |>
    rvest::html_table()


}
shot_summary <- function(game_id) {

  ss_html <- rvest::read_html('https://www.nhl.com/scores/htmlreports/20232024/SS020749.HTM')
  ss_tbls <- rvest::html_table(ss_html)


}
