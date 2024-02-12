#' @name pbp
#' @title functions for working with play by play data
#' @param game_id character string of game id
#' @param pbp_json list output from `get_pbp()`
#' @md
NULL
# exports ----
#' @rdname pbp
#' @details `get_pbp()` returns a list object from the request response
#' @export
#' @md
get_pbp <- function(game_id) {

  pbp_req <- create_request(
    base_uri=paste0('https://api-web.nhle.com/v1/gamecenter/', game_id, '/play-by-play'),
    hdrs=default_headers()
  )

  pbp_json <- httr2::req_perform(pbp_req) |>
    httr2::resp_body_json()

  return(pbp_json)

}

#' @rdname pbp
#' @details `parse_pbp()` takes a list from `get_pbp()` as input, and returns a data.frame of plays
#' @export
#' @md
parse_pbp <- function(pbp_json, verbose=FALSE) {

  # label the game_id, you'll use that a lot
  game_id <- as.character(pbp_json$id)

  # extract the plays from the pbp_json
  plays <- pbp_json$plays

  # create empty lists for the data from each play, one of indeterminate length
  play_list <- list()
  details_list <- list()

  # loop through the plays
  for (p in plays) {

    # parse the event info
    play_df <- parse_pbp_event(game_id=game_id, p)
    play_list[[length(play_list) + 1]] <- play_df
    # most plays will also have a list for details, PARSE THAT IF IT DOES
    if ('details' %in% names(p)) {

      # append to list of details
      deets <- try(parse_pbp_details(game_id = play_df$game_id,
                                     event_id = play_df$event_id,
                                     event_type_code = play_df$event_type_code,
                                     pbp_details = p[['details']]))
      if (!'try-error' %in% class(deets)) {
        details_list[[length(details_list)+1]] <- deets
      }
    }
  }

  # bind plays and details, then merge em
  all_plays <- dplyr::bind_rows(play_list)
  all_plays <- as.data.frame(lapply(all_plays, as.character))
  all_details <- dplyr::bind_rows(details_list)
  all_details <- as.data.frame(lapply(all_details, as.character))
  # merge into one big pbp df, engineer and fix some stuff
  pbp_df <- all_plays |>
    dplyr::full_join(all_details, by = dplyr::join_by(game_id, event_id)) |>
    dplyr::mutate(
      time_in_period_seconds = as.numeric(lubridate::as.duration(lubridate::ms(time_in_period))),
      time_in_game_seconds = time_in_period_seconds + (1200*(as.numeric(period)-1)))
  pbp_df[pbp_df == 'NULL'] <- NA
  return(pbp_df)

}

# unexported ----
parse_pbp_details <- function(game_id, event_id, event_type_code, pbp_details) {
  # make empty details output
  empty_pbp_df <- data.frame(
    game_id = game_id,
    event_id = event_id,
    event_type = NA_character_
  )
  output_df <- empty_pbp_df
  # handle things based on event_type_codes
  if (event_type_code==502) {
    output_df$event_type <- 'stoppage'
    output_df$event_owner_team_id <- pbp_details["eventOwnerTeamId"]
    output_df$losing_player_id <- pbp_details["losingPlayerId"]
    output_df$winning_player_id <- pbp_details["winningPlayerId"]
    output_df$x_coord <- pbp_details["xCoord"]
    output_df$y_coord <- pbp_details["yCoord"]
    output_df$zone_code <- pbp_details["zoneCode"]
  }
  if (event_type_code==503) {
    output_df$event_type <- 'hit'
    output_df$event_owner_team_id <- pbp_details["eventOwnerTeamId"]
    output_df$hitting_player_id <- pbp_details["hittingPlayerId"]
    output_df$hittee_player_id <- pbp_details["hitteePlayerId"]
    output_df$x_coord <- pbp_details["xCoord"]
    output_df$y_coord <- pbp_details["yCoord"]
    output_df$zone_code <- pbp_details["zoneCode"]
  }
  if (event_type_code==504) {
    output_df$event_type <- 'giveaway'
    output_df$shot_type <- pbp_details["shotType"]
    output_df$event_owner_team_id <- pbp_details["eventOwnerTeamId"]
    output_df$player_id <- pbp_details["playerId"]
    output_df$x_coord <- pbp_details["xCoord"]
    output_df$y_coord <- pbp_details["yCoord"]
    output_df$zone_code <- pbp_details["zoneCode"]
  }
  if (event_type_code==505) {
    output_df$event_type <- 'goal'
    output_df$shot_type <- pbp_details["shotType"]
    output_df$event_owner_team_id <- pbp_details["eventOwnerTeamId"]
    output_df$shooting_player_id <- pbp_details["scoringPlayerId"] # intentionally add this here since a score requires a shot!
    output_df$scoring_player_id <- pbp_details["scoringPlayerId"]
    output_df$assist1_player_id <- pbp_details["assist1PlayerId"]
    output_df$assist2_player_id <- pbp_details["assist2PlayerId"]
    output_df$goalie_in_net_id <- pbp_details["goalieInNetId"]
    output_df$x_coord <- pbp_details["xCoord"]
    output_df$y_coord <- pbp_details["yCoord"]
    output_df$zone_code <- pbp_details["zoneCode"]
  }
  if (event_type_code==506) {
    output_df$event_type <- 'shot_on_goal'
    output_df$shot_type <- pbp_details["shotType"]
    output_df$event_owner_team_id <- pbp_details["eventOwnerTeamId"]
    output_df$shooting_player_id <- pbp_details["shootingPlayerId"]
    output_df$goalie_in_net_id <- pbp_details["goalieInNetId"]
    output_df$x_coord <- pbp_details["xCoord"]
    output_df$y_coord <- pbp_details["yCoord"]
    output_df$zone_code <- pbp_details["zoneCode"]
  }
  if (event_type_code==507) {
    output_df$event_type <- 'missed_shot'
    output_df$shot_type <- pbp_details["shotType"]
    output_df$event_owner_team_id <- pbp_details["eventOwnerTeamId"]
    output_df$shooting_player_id <- pbp_details["shootingPlayerId"]
    output_df$goalie_in_net_id <- pbp_details["goalieInNetId"]
    output_df$x_coord <- pbp_details["xCoord"]
    output_df$y_coord <- pbp_details["yCoord"]
    output_df$zone_code <- pbp_details["zoneCode"]
    output_df$reason <- pbp_details["reason"]
  }
  if (event_type_code==508) {
    output_df$event_type <- 'blocked_shot'
    output_df$event_owner_team_id <- pbp_details["eventOwnerTeamId"]
    output_df$shooting_player_id <- pbp_details["shootingPlayerId"]
    output_df$blocking_player_id <- pbp_details["blockingPlayerId"]
    output_df$x_coord <- pbp_details["xCoord"]
    output_df$y_coord <- pbp_details["yCoord"]
    output_df$zone_code <- pbp_details["zoneCode"]
  }
  if (event_type_code==509) {
    output_df$event_type <- 'penalty'
    output_df$event_owner_team_id <- pbp_details["eventOwnerTeamId"]
    output_df$committed_by_player_id <- pbp_details["committedByPlayerId"]
    output_df$drawn_by_player_id <- pbp_details["drawnByPlayerId"]
    output_df$x_coord <- pbp_details["xCoord"]
    output_df$y_coord <- pbp_details["yCoord"]
    output_df$zone_code <- pbp_details["zoneCode"]
    output_df$penalty_type_code <- pbp_details["typeCode"]
    output_df$desc_key <- pbp_details["descKey"]
    output_df$duration <- pbp_details["duration"]
  }
  if (event_type_code==516) {
    # sometimes theres a secondary reason
    output_df$event_type <- 'faceoff'
    output_df$reason <- pbp_details["reason"]
  }
  if (event_type_code==525) {
    output_df$event_type <- 'takeaway'
    output_df$event_owner_team_id <- pbp_details["eventOwnerTeamId"]
    output_df$player_id <- pbp_details["playerId"]
    output_df$x_coord <- pbp_details["xCoord"]
    output_df$y_coord <- pbp_details["yCoord"]
    output_df$zone_code <- pbp_details["zoneCode"]

  }
  if (event_type_code==535) {
    output_df$event_type <- 'delayed_penalty'
    output_df$event_owner_team_id <- pbp_details["eventOwnerTeamId"]
  }
  if (event_type_code==537) {
    output_df$event_type <- 'shootout_complete'
    output_df$shot_type <- pbp_details["shotType"]
    output_df$event_owner_team_id <- pbp_details["eventOwnerTeamId"]
    output_df$shooting_player_id <- pbp_details["shootingPlayerId"]
    output_df$goalie_in_net_id <- pbp_details["goalieInNetId"]
    output_df$x_coord <- pbp_details["xCoord"]
    output_df$y_coord <- pbp_details["yCoord"]
    output_df$zone_code <- pbp_details["zoneCode"]
    # there are 2 SOG fields for home and away that we're omitting here, maybe need to add at some point
  }

  if(ncol(output_df)==3) browser()

  return(output_df)

}

parse_pbp_event <- function(game_id, play) {

  # make an empty event data.frame in cases where the parser breaks
  empty_event_df <- data.frame(
    game_id = game_id,
    event_id = NA_integer_,
    period = NA_character_,
    # period_descriptor = NA_character_, not taking this for now since its a weird list
    time_in_period = NA_character_,
    time_remaining = NA_character_,
    situation_code = NA_character_,
    home_team_defending_side = NA_character_,
    event_type_code = NA_character_ # adding the event specifier
  )

  # play_df is the output, starts with the empty event
  play_df <- empty_event_df

  # the play should have an eventId, but if it doesn't just return the empty df
  event_id <- try(play[['eventId']])
  if ('try-error' %in% class(event_id) || length(event_id) == 0) {
    if (verbose==TRUE) {
      message('tried parsing play without an event_id, heres what it DID have')
      message(paste(names(play), ':', play))
    }
    return(empty_event_df)
  }

  # give the play_df an event and a game
  play_df$event_id <- event_id

  # spell out event info fields we care about
  event_field_names <- c('period',
                         # 'periodDescriptor', # not taking this as an unwieldy list right now
                         'timeInPeriod',
                         'timeRemaining',
                         'situationCode',
                         'homeTeamDefendingSide',
                         'typeCode')

  # loop through event field names
  for (efn in event_field_names) {
    # if the field name isn't in the list, move on
    if (!efn %in% names(play)) next
    # loop through the names and update the values in the play_df
    val <- as.character(play[[efn]])
    if (efn=='period') play_df$period <- val
    if (efn=='periodDescriptor') play_df$period_descriptor <- val
    if (efn=='timeInPeriod') play_df$time_in_period <- val
    if (efn=='timeRemaining') play_df$time_remaining <- val
    if (efn=='situationCode') play_df$situation_code <- val
    if (efn=='homeTeamDefendingSide') play_df$home_team_defending_side <- val
    if (efn=='typeCode') play_df$event_type_code <- val
  }

  # make everything a character for simplicity
  return(play_df)
}
