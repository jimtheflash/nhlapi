boxscore_team_totals <- function(parsed_boxscore) {

  sum_rm_na <- function(x) sum(x, na.rm = TRUE)

  cols_to_sum <- c(
    "goals",
    "assists",
    "points",
    "pim",
    "hits",
    "blocked_shots",
    "power_play_goals",
    "power_play_points",
    "shorthanded_goals",
    "sh_points")

  output <- parsed_boxscore |>
    dplyr::group_by(game_id, team_id) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::any_of(cols_to_sum),
        list(team_game = sum_rm_na)
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      game_id = as.character(game_id),
      team_id = as.character(team_id)
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.numeric),
        ~tidyr::replace_na(.x, 0)
        )
      )

  return(output)

}

first_goal <- function(parsed_pbp) {

  parsed_pbp |>
    dplyr::filter(
      event_type == 'goal'
    ) |>
    dplyr::arrange(
      time_in_game_seconds
    ) |>
    dplyr::filter(dplyr::row_number()==1) |>
    dplyr::transmute(
      game_id = as.character(game_id),
      first_goal_period = as.numeric(period),
      first_goal_time_in_period_seconds = as.numeric(time_in_period_seconds),
      first_goal_time_in_game_seconds = as.numeric(time_in_game_seconds),
      first_goal_scorer = as.character(scoring_player_id)
    )

}

player_offense_by_period <- function(parsed_pbp) {

  # assists ----
  assists <- parsed_pbp |>
    dplyr::select(assist1_player_id, assist2_player_id, game_id, period, event_id) |>
    dplyr::filter(!is.na(assist1_player_id) | !is.na(assist2_player_id)) |>
    tidyr::pivot_longer(dplyr::starts_with('assist'), values_to = 'player_id') |>
    dplyr::filter(!is.na(player_id)) |>
    dplyr::group_by(player_id, game_id) |>
    dplyr::summarise(
      assists_p1 = dplyr::n_distinct(event_id[period==1]),
      assists_p2 = dplyr::n_distinct(event_id[period==2]),
      assists_p3 = dplyr::n_distinct(event_id[period==3]),
      assists_ot = dplyr::n_distinct(event_id[!period %in% c(1,2,3)])) |>
    dplyr::ungroup()

  # shots ----
  shots <- parsed_pbp |>
    dplyr::transmute(
      player_id = shooting_player_id,
      game_id,
      period,
      event_id,
      event_type
    ) |>
    dplyr::filter(!is.na(player_id)) |>
    dplyr::group_by(player_id, game_id) |>
    dplyr::summarise(
      shots_p1 = dplyr::n_distinct(event_id[period==1]),
      shots_on_goal_p1 = dplyr::n_distinct(event_id[period==1 & event_type=='shot_on_goal']),
      shots_missed_p1 = dplyr::n_distinct(event_id[period==1 & event_type=='missed_shot']),
      shots_p2 = dplyr::n_distinct(event_id[period==2]),
      shots_on_goal_p2 = dplyr::n_distinct(event_id[period==2 & event_type=='shot_on_goal']),
      shots_missed_p2 = dplyr::n_distinct(event_id[period==2 & event_type=='missed_shot']),
      shots_p3 = dplyr::n_distinct(event_id[period==3]),
      shots_on_goal_p3 = dplyr::n_distinct(event_id[period==3 & event_type=='shot_on_goal']),
      shots_missed_p3 = dplyr::n_distinct(event_id[period==3 & event_type=='missed_shot']),
      shots_ot = dplyr::n_distinct(event_id[!period %in% c(1,2,3)]),
      shots_on_goal_ot = dplyr::n_distinct(event_id[!period %in% c(1,2,3) & event_type=='shot_on_goal']),
      shots_missed_ot = dplyr::n_distinct(event_id[!period %in% c(1,2,3) & event_type=='missed_shot'])
      ) |>
    dplyr::ungroup()

  # goals ----
  goals <- parsed_pbp |>
    dplyr::transmute(
      player_id = scoring_player_id,
      game_id,
      period,
      event_id,
      event_type,
    ) |>
    dplyr::filter(!is.na(player_id)) |>
    dplyr::group_by(player_id, game_id) |>
    dplyr::summarise(
      goals_p1 = dplyr::n_distinct(event_id[period==1]),
      goals_p2 = dplyr::n_distinct(event_id[period==2]),
      goals_p3 = dplyr::n_distinct(event_id[period==3]),
      goals_ot = dplyr::n_distinct(event_id[!period %in% c(1,2,3)]),
      unassisted_goals_p1 = dplyr::n_distinct(event_id[is.na(assist1_player_id)&period==1]),
      unassisted_goals_p2 = dplyr::n_distinct(event_id[is.na(assist1_player_id)&period==2]),
      unassisted_goals_p3 = dplyr::n_distinct(event_id[is.na(assist1_player_id)&period==3]),
      unassisted_goals_ot = dplyr::n_distinct(event_id[is.na(assist1_player_id)&!period %in% c(1,2,3)])) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      unassisted_goals = unassisted_goals_p1 + unassisted_goals_p2 + unassisted_goals_p3 + unassisted_goals_ot
    )

  # join em up
  joined <- assists |>
    dplyr::full_join(shots, by = dplyr::join_by(player_id, game_id)) |>
    dplyr::full_join(goals, by = dplyr::join_by(player_id, game_id)) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::ends_with('_id'),
        as.character
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.numeric),
        ~tidyr::replace_na(.x, 0)
      )
    )



  joined[is.na(joined)] <- 0

  return(joined)

}

player_shifts_stats_by_period <- function(shifts) {

  # find starters
  starters <- unique(shifts$playerId[shifts$period==1&shifts$startTime=="00:00"])

  # shifts and toi
  shift_summary <- shifts |>
    dplyr::filter(!is.na(playerId)) |>
    dplyr::mutate(
      player_id = as.character(playerId),
      game_id = as.character(gameId),
      shift_start_period_elapsed_seconds = as.numeric(lubridate::as.duration(lubridate::ms(startTime))),
      shift_duration_seconds = as.numeric(lubridate::as.duration(lubridate::ms(duration)))
    ) |>
    dplyr::group_by(
      player_id, game_id
    ) |>
    dplyr::summarise(
      shifts_p1 = dplyr::n_distinct(shiftNumber[period==1]),
      shifts_p2 = dplyr::n_distinct(shiftNumber[period==2]),
      shifts_p3 = dplyr::n_distinct(shiftNumber[period==3]),
      shifts_ot = dplyr::n_distinct(shiftNumber[!period %in% c(1,2,3)]),
      toi_seconds_p1 = sum(shift_duration_seconds[period==1], na.rm = TRUE),
      toi_seconds_p2 = sum(shift_duration_seconds[period==2], na.rm = TRUE),
      toi_seconds_p3 = sum(shift_duration_seconds[period==3], na.rm = TRUE),
      toi_seconds_ot = sum(shift_duration_seconds[!period %in% c(1,2,3)], na.rm = TRUE)) |>
    dplyr::ungroup()

  # annoyingly, it is easier to get ppgs from here than other places so far
  pp_goals <- shifts |>
    dplyr::mutate(
      player_id = as.character(playerId)
    ) |>
    dplyr::filter(
      eventDescription=='PPG'
    ) |>
    dplyr::group_by(
      player_id
    ) |>
    dplyr::summarise(
      goals_from_power_play_p1 = dplyr::n_distinct(id[period==1]),
      goals_from_power_play_p2 = dplyr::n_distinct(id[period==2]),
      goals_from_power_play_p3 = dplyr::n_distinct(id[period==3]),
      goals_from_power_play_ot = dplyr::n_distinct(id[!period %in% c(1,2,3)])
    ) |>
    dplyr::ungroup()

  # combine for output
  output <- shift_summary |>
    dplyr::left_join(pp_goals, by='player_id') |>
    dplyr::mutate(starter_ind = dplyr::if_else(player_id %in% starters, 1, 0)) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::ends_with('_id'),
        as.character
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.numeric),
        ~tidyr::replace_na(.x, 0)
      )
    )

  return(output)

}
