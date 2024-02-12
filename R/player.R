get_all_players <- function(fn='nhl_players.csv') {
  read.csv(system.file(fn, package='nhlapi'))
}

get_all_players_from_api <- function(write_to_package=TRUE, fn='nhl_players.csv') {

  resp <- jsonlite::fromJSON('https://search.d3.nhle.com/api/v1/search/player?culture=en-us&limit=999999&q=%2A')
  resp$update_ts <- Sys.time()
  write.csv(resp, file = filepath, quote = FALSE, row.names = FALSE)

}
