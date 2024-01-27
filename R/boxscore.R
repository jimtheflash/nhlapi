get_boxscore <- function(game_id) {

  boxscore_req <- create_request(
    base_uri=paste0('https://api-web.nhle.com/v1/gamecenter/', game_id, '/boxscore'),
    hdrs=default_headers()
  )

  httr2::req_perform(boxscore_req) |>
    httr2::resp_body_json()

}

parse_boxscore <- function(boxscore_json) {

  browser()
  # extract the boxscore
  boxscore_list <- boxscore_json$boxscore

  # loop through teams and positions to get player stats by game
  player_stats <- boxscore_list$playerByGameStats
  team_output_list <- replicate(length(player_stats), list())
  names(team_output_list) <- names(player_stats)
  for (team in names(player_stats)) {

    team_stats <- player_stats[[team]]


  }



  return(boxscore_df)

}
