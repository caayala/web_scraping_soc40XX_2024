## -----------------------------------------------------------------------------
#| echo = FALSE
suppressPackageStartupMessages(library(tidyverse))
library(rvest)
library(httr2)
library(knitr)

opts_chunk$set(cache.path = "class_9_files/class_9_cache/html/")


## -----------------------------------------------------------------------------
req_wiki_api <- request('https://es.wikipedia.org') |> 
  req_url_path('w/api.php') |> 
  req_headers('Accept-Encoding' = 'gzip')

req_wiki_search <- req_wiki_api |>
  req_url_query(!!!list(action = 'query',
                        list = 'search',
                        srsearch = 'web scraping',
                        srlimit = 5,
                        format = 'json'))

resp_wiki_search <- req_perform(req_wiki_search)

# Leo el json que está dentro de la respuesta a la petición.
df <- resp_wiki_search |> 
  resp_body_json(simplifyVector = T)

df |> str()


## -----------------------------------------------------------------------------
df$query$search |> as_tibble()


## -----------------------------------------------------------------------------
req_wiki_parse <- req_wiki_api |>
  req_url_query(!!!list(action = 'parse',
                        page = 'Web scraping', # contenido de título de página
                        prop = 'text', # html de retorno
                        format = 'json'))

resp_wiki_parse <- req_perform(req_wiki_parse)

df_wiki_parse <- resp_wiki_parse |> 
  resp_body_json(simplifyVector = T)

df_wiki_parse |> str(3)


## -----------------------------------------------------------------------------
df_wiki_parse$parse$text$`*` |> 
  read_html() |> html_text() |> 
  str_trunc(width = 1e3) |> cat()


## -----------------------------------------------------------------------------
req_wiki_query <- req_wiki_api |>
  req_url_query(!!!list(action = 'query',
                         titles = 'Web scraping|Python', # Varios títulos a la ves.
                         prop = 'info|categories|iwlinks', # Información a obtener.
                         format = 'json'))

resp_wiki_query <- req_perform(req_wiki_query)

df_wiki_query <- resp_wiki_query |> 
  resp_body_json(simplifyVector = T)

df_wiki_query |> str(3)


## -----------------------------------------------------------------------------
df_wiki_query$query$pages |> 
  enframe() |> 
  unnest_wider(value)


## -----------------------------------------------------------------------------
library(WikipediR)

info_wiki <- page_info(
  language = 'es', 
  project = 'wikipedia', 
  page = 'Web_scraping|Python') 


## -----------------------------------------------------------------------------
str(info_wiki, 3)


## -----------------------------------------------------------------------------
info_wiki$query$pages |> 
  enframe() |> 
  unnest_wider(value)


## -----------------------------------------------------------------------------
cont_wiki <-  page_content(language = 'es', 
                           project = 'wikipedia', 
                           page_name = 'Web_scraping')

str(cont_wiki) # Una lista parse con 4 elementos dentro


## -----------------------------------------------------------------------------
cont_wiki$parse$text$`*` |> # Texto del requerimiento
  read_html() |> html_text() |> 
  str_trunc(width = 1e3) |> cat()


## -----------------------------------------------------------------------------
req_spotify_api <- request('https://api.spotify.com') |> 
  req_url_path('v1') |> 
  req_headers(Authorization = str_glue("Bearer {spotifyr::get_spotify_access_token()}"),
              Accept = 'application/json"',
              'Accept-Encoding' = 'gzip')

req_spotify_met <- req_spotify_api |> 
  req_url_path_append('search') |> 
  req_url_query(q = 'genre:metal', 
                market = 'CL', 
                type = 'artist', 
                limit = 8)

resp_spotify_met <- req_spotify_met |> req_perform()

df_spotify_met <- resp_spotify_met |> 
  resp_body_json(simplifyVector = T)

df_spotify_met$artists$items |> 
  as_tibble() |> unpack(followers, names_repair = 'minimal') |>
  select(id, name, popularity, total)


## -----------------------------------------------------------------------------
#| eval: false
#| echo: false
#|
# Para pedir un token directamente por la API.
# Debemos agregar información en el body del request.
req_spotify_token <- request('https://accounts.spotify.com') |> 
  req_url_path('api/token') |> 
  req_auth_basic(username = Sys.getenv('SPOTIFY_CLIENT_ID'),
                 password = Sys.getenv('SPOTIFY_CLIENT_SECRET')) |> 
  req_body_form(grant_type = "client_credentials")

resp_spotify_token <- req_spotify_token |> req_perform()

resp_spotify_token |> resp_body_json()


## -----------------------------------------------------------------------------
library(spotifyr)

df_met <- get_genre_artists('metal', limit = 8)
df_met |> 
  select(id, name, popularity, followers.total)


## -----------------------------------------------------------------------------
df_pod <- search_spotify(q = 'podcast', type = 'show', market = 'CL', limit = 10)

df_pod |> 
  select(id, name, total_episodes, description)


## -----------------------------------------------------------------------------
df_top <- get_playlist('37i9dQZEVXbL0GavIqMTeb') # ID de la lista.
df_top_track <- df_top$tracks$items |> as_tibble()

suppressMessages(
  # Seleccionar y limpiar solo alguna de las variables disponibles
  df_top_track_sel <- df_top_track |> 
    mutate(track.id, track.name, track.popularity, track.album.release_date, 
           name = map(track.album.artists, 'name'),
           .keep = 'none') |> 
    unnest_wider(col = name, names_sep = '_')
)

df_top_track_sel |> head()


## -----------------------------------------------------------------------------
df_track_af <- get_track_audio_features(df_top_track_sel$track.id)

head(df_track_af)


## -----------------------------------------------------------------------------
df_top_track_sel_gg <- bind_cols(df_top_track_sel, 
                                 df_track_af) |> 
  arrange(track.popularity) |> 
  mutate(pos = row_number())

gg <- df_top_track_sel_gg |> 
  pivot_longer(cols = c(danceability, 
                        energy, 
                        speechiness, 
                        acousticness, 
                        liveness),
               names_to = 'variable', 
               values_to = 'valor') |> 
  ggplot(aes(x = variable, 
             y = valor, 
             colour = track.popularity)) +
  geom_violin(colour = 'gray80') + 
  geom_point(aes(size = pos),
             alpha = 0.3,
             show.legend = FALSE) + 
  scale_colour_steps(low = '#266591', high = '#f57500',
                      ) +
  scale_radius() +
  theme_minimal() +
  labs(title = 'Características de las 50 canciones en Chile',
       subtitle = 'Lista Top 50 — Chile, Spotify', 
       x = NULL)


## -----------------------------------------------------------------------------
#| fig-height: 7
gg


## -----------------------------------------------------------------------------
library(rtoot)

custom_instance <- 'lile.cl'

lile_instance <- get_instance_general(instance = custom_instance, anonymous = TRUE)

lile_instance |> str(2)


## -----------------------------------------------------------------------------
lile_peers <- get_instance_peers(instance = custom_instance, token = NULL, anonymous = TRUE)

head(lile_peers)


## -----------------------------------------------------------------------------
lile_peers[grepl('chile', lile_peers)]


## -----------------------------------------------------------------------------
lile_peers[grepl('mastodon.social', lile_peers)]


## -----------------------------------------------------------------------------
get_instance_trends(
  instance = custom_instance,
  token = NULL,
  limit = 10,
  anonymous = TRUE
)


## -----------------------------------------------------------------------------
l_toot_chile <- rtoot(
  endpoint = '/api/v2/search',
  params = list(q = 'chile',
                type = 'accounts',
                limit = 40), # puede ser hashtags, statuses
  token = Sys.getenv('MSTDN_SOCIAL_KEY'),
  instance = 'mstdn.social',
  anonymous = FALSE)

df_toot_chile <- l_toot_chile$accounts |> 
  map(\(x) unlist(x) |> t() |> data.frame()) |> 
  reduce(bind_rows)

df_toot_chile$username


## -----------------------------------------------------------------------------
if(interactive()) {
  stream_timeline_public(
    timeout = 30,
    local = FALSE,
    file_name = 'slides/class_9/class_9_files/stream_lile_public.json',
    append = TRUE,
    instance = custom_instance,
    token = NULL,
    anonymous = FALSE,
    verbose = TRUE
  )
  
  parse_stream("slides/class_9/class_9_files/stream_lile_public.json")
}


## -----------------------------------------------------------------------------
#| include: false

# Extraer código R
knitr::purl('class_9.qmd',
            output = 'class_9.R',
            quiet = TRUE)

