get_week_schedule_dates <- function(season, sleep_time=5, verbose=FALSE) {

  # get the lookup from the package, use the function to get the right start_date
  season_start_dates <- season_start_dates_lu()
  start_date <- season_start_dates$season_start_date[season_start_dates$season == season]

  # hit the api to get the preseason start date and playoff end date
  initial_schedule_json <- get_week_schedule(start_date)
  preseason_start_date <- as.Date(initial_schedule_json$preSeasonStartDate)
  playoff_end_date <- as.Date(initial_schedule_json$playoffEndDate)

  # starting with the first preseason, grab all the week start dates til the playoffs end
  date_list <- c()
  week_start_date <- preseason_start_date
  while (week_start_date <= playoff_end_date) {
    if (verbose==TRUE) message(week_start_date)
    # append the date to the date list
    date_list[[length(date_list)+1]] <- week_start_date
    # snooze then pull the schedule for that week, to get the following week schedule
    Sys.sleep(sleep_time)
    week_schedule <- get_week_schedule(week_start_date)
    # overwrite the week start date with upcoming week, kick back to the top of the loop
    week_start_date <- as.Date(week_schedule$nextStartDate)
  }

  return(date_list)

}


get_week_schedule <- function(game_date) {

  schedule_req <- create_request(
    base_uri=paste0('https://api-web.nhle.com/v1/schedule/', game_date),
    hdrs=default_headers()
  )

  schedule_json <- httr2::req_perform(schedule_req) |>
    httr2::resp_body_json()

}

parse_week_schedule <- function(schedule_json) {

  # grab the game week
  output_list <- list()
  game_week <- schedule_json$gameWeek

  # game_week should be a list of days for the week schedule with games by day
  daily_games <- list()
  for (gw in game_week) {
    # extract the games, but if there aren't any then skip
    games <- gw$games
    if (length(games)==0) next

    # go through the games and make outputs
    game_output <- replicate(length(games), list())
    for (i in seq_along(games)) {

      g <- games[[i]]

      # make a df of the stuff we want, ie schedule info (not outcomes)
      game_df <- data.frame(
        game_id = g$id,
        season = g$season,
        game_type = g$gameType,
        start_time_utc = g$startTimeUTC,
        away_team_abbrev = g$awayTeam$abbrev,
        away_team_id = g$awayTeam$id,
        home_team_abbrev = g$homeTeam$abbrev,
        home_team_id = g$homeTeam$id
      )

      game_output[[i]] <- game_df

    }

    # stitch together the game outputs
    games_df <- dplyr::bind_rows(game_output)
    daily_games[[length(daily_games)+1]] <- games_df

  }

  # stitch together all the days and return
  week_schedule_df <- dplyr::bind_rows(daily_games)

  return(week_schedule_df)

}
