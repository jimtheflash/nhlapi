#' boxscore funtions
#' @param game_id character game id from nhl
#' @param boxscore_json list output from `get_boxscore`
#' @export
get_boxscore <- function(game_id) {

  boxscore_req <- create_request(
    base_uri=paste0('https://api-web.nhle.com/v1/gamecenter/', game_id, '/boxscore'),
    hdrs=default_headers()
  )

  boxscore_json <- httr2::req_perform(boxscore_req) |>
    httr2::resp_body_json()

  return(boxscore_json)

}

parse_boxscore <- function(boxscore_json) {

  # extract the boxscore
  boxscore_list <- boxscore_json$boxscore

  # loop through teams and positions to get player stats by game
  player_stats <- boxscore_list$playerByGameStats
  team_labels <- names(player_stats)
  team_output_list <- replicate(length(team_labels), list())
  names(team_output_list) <- team_labels
  for (team in team_labels) {

    # subset team stats
    team_stats <- player_stats[[team]]

    # subloop for positions
    position_labels <- names(team_stats)
    position_output_list <- replicate(length(position_labels), list())
    names(position_output_list) <- position_labels
    for (pos in position_labels) {

      pos_group <- team_stats[[pos]]
      # players have multiple names and maybe sweaters, we just care about their ID so use that to group obs
      pos_df <- dplyr::bind_rows(pos_group) |>
        dplyr::group_by(playerId) |>
        dplyr::summarise(
          dplyr::across(
            dplyr::everything(),
            dplyr::first
          )
        ) |>
        dplyr::ungroup()

      pos_df$position_group <- pos
      position_output_list[[pos]] <- pos_df

    }

    all_positions_df <- dplyr::bind_rows(position_output_list)

    # append team info
    all_positions_df$teamId <- boxscore_json[[team]][['id']]
    all_positions_df$teamAbbrev <- boxscore_json[[team]][['abbrev']]

    # stick in the list
    team_output_list[[team]] <- all_positions_df

  }

  # most of the column names are recycled across pos groups, so bind em all together
  boxscore_df <- dplyr::bind_rows(team_output_list)

  # append game info, but not too much, cuz a game object will have that
  boxscore_df$gameId <- boxscore_json$id
  boxscore_df$season <- boxscore_json$season
  boxscore_df$gameType <- boxscore_json$gameType
  boxscore_df$game_date <- boxscore_json$gameDate

  return(boxscore_df)

}

build_season_boxscores_df <- function(season, sleep_time=5, verbose=FALSE) {

  # get all the week starts for the season, sleeping appropriately
  week_start_dates <- get_week_schedule_dates(season, sleep_time=sleep_time, verbose=verbose)

  # roll through th weeks and get boxscore data
  parsed_boxscores <- list()
  for (week_start_date in week_start_dates) {
    if (verbose==TRUE) message('getting games for week starting ', week_start_date)
    # grab week schedule; we'll sleep between boxscores
    week_schedule <- get_week_schedule(week_start_date) |>
      parse_week_schedule()
    game_ids <- unique(week_schedule$game_id)
    for (game_id in game_ids) {
      if (verbose==TRUE) message('grabbing boxscore for ', game_id)
      Sys.sleep(sleep_time)
      parsed_boxscore <- get_boxscore(game_id) |>
        parse_boxscore()
      parsed_boxscores[[length(parsed_boxscores) + 1]] <- parsed_boxscore
    }
  }

  season_boxscores_df <- dplyr::bind_rows(parsed_boxscores)

  return(season_boxscores_df)





}
