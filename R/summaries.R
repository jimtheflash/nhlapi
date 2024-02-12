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
      game_id,
      first_goal_period = period,
      first_goal_time_in_period_seconds = time_in_period_seconds,
      first_goal_time_in_game_seconds = time_in_game_seconds,
      first_goal_scorer = scoring_player_id
    )

}

player_offense_by_period <- function(parsed_pbp) {

  # assists ----
  assists <- parsed_pbp |>
    dplyr::select(assist1_player_id, assist2_player_id, period, event_id) |>
    dplyr::filter(!is.na(assist1_player_id) | !is.na(assist2_player_id)) |>
    tidyr::pivot_longer(dplyr::starts_with('assist'), values_to = 'player_id') |>
    dplyr::filter(!is.na(player_id)) |>
    dplyr::group_by(player_id) |>
    dplyr::summarise(
      assists_p1 = dplyr::n_distinct(event_id[period==1]),
      assists_p2 = dplyr::n_distinct(event_id[period==2]),
      assists_p3 = dplyr::n_distinct(event_id[period==3]),
      assists_ot = dplyr::n_distinct(event_id[!period %in% c(1,2,3)]))

  # shots ----
  shots <- parsed_pbp |>
    dplyr::transmute(
      player_id = shooting_player_id,
      period,
      event_id,
      event_type
    ) |>
    dplyr::filter(!is.na(player_id)) |>
    dplyr::group_by(player_id) |>
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
      )

  # goals ----
  goals <- parsed_pbp |>
    dplyr::transmute(
      player_id = scoring_player_id,
      period,
      event_id,
      event_type,
    ) |>
    dplyr::filter(!is.na(player_id)) |>
    dplyr::group_by(player_id) |>
    dplyr::summarise(
      goals_p1 = dplyr::n_distinct(event_id[period==1]),
      goals_p2 = dplyr::n_distinct(event_id[period==2]),
      goals_p3 = dplyr::n_distinct(event_id[period==3]),
      goals_ot = dplyr::n_distinct(event_id[!period %in% c(1,2,3)]))

  # join em up
  joined <- assists |>
    dplyr::full_join(shots, by = 'player_id') |>
    dplyr::full_join(goals, by = 'player_id')

  return(joined)

}

player_shifts_stats_by_period <- function(shifts) {

  # find starters
  starters <- unique(shifts$playerId[shifts$period==1&shifts$startTime=="00:00"])

  # shifts and toi
  shift_summary <- shifts |>
    dplyr::mutate(
      player_id = as.character(playerId),
      shift_start_period_elapsed_seconds = as.numeric(lubridate::as.duration(lubridate::ms(startTime))),
      shift_duration_seconds = as.numeric(lubridate::as.duration(lubridate::ms(duration)))
    ) |>
    dplyr::group_by(
      player_id
    ) |>
    dplyr::summarise(
      shifts_p1 = dplyr::n_distinct(shiftNumber[period==1]),
      shifts_p2 = dplyr::n_distinct(shiftNumber[period==2]),
      shifts_p3 = dplyr::n_distinct(shiftNumber[period==3]),
      shifts_ot = dplyr::n_distinct(shiftNumber[!period %in% c(1,2,3)]),
      toi_seconds_p1 = sum(shift_duration_seconds[period==1], na.rm = TRUE),
      toi_seconds_p2 = sum(shift_duration_seconds[period==2], na.rm = TRUE),
      toi_seconds_p3 = sum(shift_duration_seconds[period==3], na.rm = TRUE),
      toi_seconds_ot = sum(shift_duration_seconds[!period %in% c(1,2,3)], na.rm = TRUE))

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
    )

  # combine for output
  output <- shift_summary |>
    dplyr::left_join(pp_goals, by='player_id')

  # replace NAs with 0s
  output[is.na(output)] <- 0

  # id starters
  output$starter_ind <- ifelse(output$player_id %in% starters, 1, 0)

  return(output)

}