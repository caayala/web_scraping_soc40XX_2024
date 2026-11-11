# Load the packages
library(httr2)
library(spotifyr)
library(tidyverse)


# Usando spotifyr ----

# beatles <- get_artist_audio_features('the beatles')

scope_valid <- scopes()[c(7, 8, 9, 10, 14, 15)]
redirect_uri <- "http://localhost:1410/"


# Problema al abrir esto en Safari.
spotify_code <- get_spotify_authorization_code(
  client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
  client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
  # https://github.com/charlie86/spotifyr/issues/198
  scope = scope_valid
)

# df_50 <- get_my_recently_played(limit = 50,
#                                 authorization = spotify_code)
#
# my_id <- 'caayala'
# my_plists <- get_user_playlists(my_id,
#                                 authorization = spotify_code)

# Define Spotify API credentials

# access_token <- spotify_code$credentials$access_token
# spotify_code$credentials |> str()
# spotify_code$app|> str()

# req_auth <- request('https://accounts.spotify.com/') |>
#   req_url_path('authorize') |>
#   req_url_query(response_type = 'code',
#                 client_id = Sys.getenv('SPOTIFY_CLIENT_ID'),
#                 redirect_uri = redirect_uri,
#                 scope = 'user-read-recently-played')

oauth_client_spotifyr <- oauth_client(
  id = Sys.getenv('SPOTIFY_CLIENT_ID'),
  secret = Sys.getenv('SPOTIFY_CLIENT_SECRET'),
  token_url = 'https://accounts.spotify.com/api/token'
)

req_auth <- request('https://accounts.spotify.com/') |>
  req_url_path('authorize') |>
  req_oauth_auth_code(
    client = oauth_client_spotifyr,
    auth_url = 'https://accounts.spotify.com/authorize',
    redirect_uri = redirect_uri,
    scope = paste0(scope_valid, collapse = ' ')
  )

req_auth |> req_dry_run()

# No funciona bien en Safari en este momento.
resp_auth <- req_auth |>
  req_perform()

resp_auth |>
  resp_body_html()

access_token <- resp_auth$request$headers$Authorization
# access_token <- 'AQC0PwFN2ZFW0nMkUpMdkdbddLJlrZSY7LZCwQxZdHUAuNUKsBDh3IrIwajkmHHhgYItYnD4PI5LbjJaJFsOpLliSSA_w3ft0VNM4OEbfLlI1ELRKZ1Kiewquat_TTj1GJz7wDX0WP34NWf1jpf-bLqzbVmH0vOWahK0emkOuMpZ64xQYb5wwiPRKCHFQ3Pvhvsxc0sdDjKnqndYNIa5NrcM60kJg0xb--6IbYg5OYjoWysDXRqQbXCu2kHuhRCf5Cq0kggIAlbI4VuNqrU4W_76WeoSegQ7hre_UWbBYccf_heYWfAXvp5lG0Coc8M6S_AUVGRVx7qpmo9hKAsvFhc133sgwRe37JTqCUn2mbznt2q3kocM6T5rnf0BfQIns0MnnJBvy_--LAfP8dwFY8fm29iyRSSsQ347VQ&state=j3eLF7NK-Y9pH23CEANH1D8omM_na2J7LNYarWxtEv8'

## Define Functions to Interact with the Spotify API ----

# Function to get recently played tracks
# solo baja las ultimas 50 canciones.
get_recently_played <- function(bearer, limit = 50, before = NULL) {
  url <- "https://api.spotify.com/v1/me/player/recently-played"

  query_params <- list(limit = limit, before = before)

  req <- request(url) %>%
    req_headers(Authorization = bearer) %>%
    req_url_query(!!!query_params)

  req |> req_dry_run()

  response <- req |>
    req_perform()

  response |>
    resp_body_json(simplifyVector = FALSE)
}

df_50a <- get_recently_played(access_token)

df_50a$`next`

# No hay más.
df_50b <- get_recently_played(access_token, before = 1720743081840)
df_50b$items


# Top ----

get_top_user <- function(
  bearer,
  limit = 50,
  type = 'tracks',
  time_range = 'medium_term', # 6 meses
  offset = 0
) {
  url <- "https://api.spotify.com/v1/me/top"

  query_params <- list(limit = limit, time_range = time_range, offset = offset)

  req <- request(url) |>
    req_url_path_append(type) |>
    req_headers(Authorization = bearer) |>
    req_url_query(!!!query_params)

  req |> req_dry_run()

  response <- req |>
    req_perform()

  response |>
    resp_body_json(simplifyVector = FALSE)
}

df_top_tracks_month <- get_top_user(access_token)
df_top_tracks_month2 <- get_top_user(access_token, offset = 50)


df_top_tracks_month$`next`
df_top_tracks_month2$`next`

df_top_tracks_month |> str(2)
df_top_tracks_month$items |> str(2)

l_top_songs <- c(df_top_tracks_month$items, df_top_tracks_month2$items)

df_top_songs <- l_top_songs |>
  map(enframe) |>
  map(\(x) pivot_wider(x, names_from = name, values_from = value)) |>
  list_rbind()

df_top_songs <- df_top_songs |>
  mutate(
    across(c(id, name, href, uri), as.character),
    across(c(disc_number, duration_ms, popularity), as.integer)
  )

df_top_songs |>
  count(name)


df_top_songs_audio <- spotifyr::get_track_audio_features(
  ids = df_top_songs$id,
  authorization = spotifyr::get_spotify_access_token()
)

df_top_songs_audio2 <- bind_cols(
  df_top_songs,
  df_top_songs_audio |>
    select(setdiff(names(df_top_songs_audio), names(df_top_songs)))
)

df_top_songs_audio2 |>
  saveRDS('slides/class_9/class_9_files/df_top_songs_audio_caayala.rds')


df_top_songs_audio2 |>
  arrange(energy) |>
  select(name, popularity, energy)
