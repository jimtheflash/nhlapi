get_pbp <- function(game_id) {

  pbp_req <- create_request(
    base_uri=paste0('https://api-web.nhle.com/v1/gamecenter/', game_id, '/play-by-play'),
    hdrs=default_headers()
  )

  pbp_json <- httr2::req_perform(pbp_req) |>
    httr2::resp_body_json()

  return(pbp_json)

}

parse_pbp <- function(pbp_json) {

  # extract the plays from the pbp_json
  plays <- pbp_json$plays

  # create empty lists for the data from each play, one of indeterminate length
  event_list <- as.list(replicate(length(plays), data.frame()))
  details_list <- list()

  # loop through the plays
  for (i in seq_along(plays)) {

    # extract play from list
    p <- plays[[i]]

    # every play should have these fields
    event_df <- data.frame(

      eventId = p[['eventId']],
      period = p[['period']],
      periodDescriptor = p[['periodDescriptor']],
      timeInPeriod = p[['timeInPeriod']],
      timeRemaining = p[['timeRemaining']],
      situationCode = p[['situationCode']],
      homeTeamDefendingSide = p[['homeTeamDefendingSide']],
      typeCode = p[['typeCode']]

    )

    # stash the list of stuff everybody should have
    event_list[[i]] <- event_df

    # most plays will also have a field for details
    if ('details' %in% names(p)) {

      # extract the details list
      details <- p[['details']]
      # append the eventId so it can be merged later
      details[['eventId']] <- p[['eventId']]
      # append to list of details
      details_list[[length(details_list)+1]] <- details

    }

  }

  # bind all the events together into one big df
  all_events_df <- dplyr::bind_rows(event_list)

  # bind all the details into a big df
  all_details_df <- dplyr::bind_rows(details_list)

  # merge into one big pbp df and return
  pbp_df <- dplyr::full_join(all_events_df, all_details_df, by = 'eventId')

  return(pbp_df)

}

tidy_pbp <- function(parsed) {
  return()
}
