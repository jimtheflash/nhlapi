get_active_players <- function() {

  active_players_req <- create_request('https://search.d3.nhle.com/api/v1/search/player',
                                       hdrs=default_headers(),
                                       bdy=list(limit=99999,
                                                culture='en-us',
                                                active='true',
                                                q='%2A'))
  active_players_json <- httr2::req_perform(active_players_req)

  |>
    httr2::resp_body_json()





}

get_player_gamelogs <- function(player_id, game_id) {}
