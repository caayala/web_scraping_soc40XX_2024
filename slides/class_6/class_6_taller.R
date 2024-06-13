# title: "Lecturas de sitios web"
# subtitle: "Web scraping y acceso a datos desde la web"
# author: Cristián Ayala
# date: 2024-06-13

suppressPackageStartupMessages(library(tidyverse))
library(httr2)
library(rvest)

# Ejemplo 1: Moodle Ciencias Sociales UC ----
# 
# Login a sitio web

url_outh <- 'https://cursosonline.cienciassociales.uc.cl/login/index.php'

## Usando rvest ----
# 
# Creamos una sesión en con las credenciales de ingreso
s <- session(url = url_outh)

(s_form <- html_form(s)[[1]])

# Agregar al formulario los datos correspondientes.
s_form <- html_form_set(s_form,
                        username = Sys.getenv('MOODLE_CCSS_UC_ID'),
                        password = Sys.getenv('MOODLE_CCSS_UC_PASS'))

# Autentificación en la sesión s.
session_submit(s, s_form)


### Participantes ----

# Link para participantes
# 
# Descomposición de un link.
url_captura <- 'https://cursosonline.cienciassociales.uc.cl/user/index.php?id=66'
httr2::url_parse(url_captura)

# Crear link para capturar participantes del curso
# 
# URL base
url_portal_render <- httr2::url_parse('https://cursosonline.cienciassociales.uc.cl')
# path
url_portal_render$path <- 'user/index.php'
# variables
url_portal_render$query <- list(id = 66)

# Composición de URL
(url_portal_render_web <- url_build(url_portal_render))

# Salto desde sesión a la URL recién construida.
html_web_scraping <- session_jump_to(x = s,
                                     url = url_portal_render_web)

# Tabla de participantes.
html_web_scraping |> 
  read_html() |>
  html_elements('.table-dynamic') |> 
  html_table()


### Mis cursos ----

url_captura <- 'https://cursosonline.cienciassociales.uc.cl/?redirect=0'
url_parse(url_captura)

html_web_scraping_categoria <- session_jump_to(s,
                                               url_captura)

l_categorias <- html_web_scraping_categoria |> 
  read_html() |>
  html_elements('.position-relative .rui-course-card')

df <- tibble(titulo = l_categorias |> 
               html_element('.mb-1') |> 
               html_text2(),
             diplomado = l_categorias |> 
               html_element('.mt-2 ') |> 
               html_text2(),
             link = l_categorias |> 
               html_element('a') |> 
               html_attr('href'))

df |> 
  head(20)

# Ejemplo 2: Ciudades Amigables ----
# 

# EJERCICIO:
# Capturar la información de esta página
url_home <- 'https://www.ciudadesamigables.cl/comunas-amigables/'

# Certificado de SSH inválido. No ha sido renovado.
try(
  rvest::session(url_home)
)

# Puedo leer la web evitando la verificación del certificado.

home_s <- request(url_home) |> 
  req_options(ssl_verifypeer = FALSE) |> 
  req_cookie_preserve('slides/class_6/class_6_files/resp_comunas_cookies') |> 
  req_perform()

resp_status(home_s)

# Captura del contenido en formato html.
home_html <- home_s |> 
  resp_body_html()


# EJERCICIO:
# ¿Está la tabla de interés?

# No encontramos lo que buscamos:
home_html |> 
  html_elements('.overflow-auto')
  
# No hay un formulario.
html_form(home_html)


# EJERCICIO:
# Buscar el *request* que entrega la información con los datos municipales.

# El servidor entrega resultados desde:
url_consulta <- 'https://www.ciudadesamigables.cl/api/comunas-filters/'

home_input <- home_html |> 
  html_elements('input')

home_input

# EJERCICIO:
# Construir y capturar ese link. ¿Qué sucede?

web_token <- setNames(home_input |> 
                        html_attr('value'),
                      paste0('X-', 
                             home_input |> 
                               html_attr('name')))

req_consulta <- request(url_consulta) |> 
  req_headers('X-Requested-With' = 'XMLHttpRequest',
              !!!web_token,
              Accept = 'application/json') |> 
  req_url_query(page = 1,
                status = '1|2|3|4|5|6',
                cycle = '1|2') |> 
  req_cookie_preserve('slides/class_6/class_6_files/resp_comunas_cookies') |> 
  req_options(ssl_verifypeer = FALSE)

# Probamos el request.
req_consulta |> 
  req_dry_run()

# Pedimos la información
resp_data <- req_consulta |> 
  req_perform()

# lectura del contenido de la respuesta. Formato json
json_comunas <- resp_data |> 
  resp_body_json()

json_comunas |> 
  str(2)


# Tabla con información de ciudad.
json_comunas$comunas |> 
  map(as_tibble) |> 
  list_rbind()


## Captura de toda la información de las comunas ----

# Páginas de comunas
pages <- 1:39

# Función para capturar la información

# EJERCICIO:
# Construir función para capturar las sucesivas páginas con información.

json_page <- function(.page){
  req_consulta <- request(url_consulta) |> 
    req_headers('X-Requested-With' = 'XMLHttpRequest',
                !!!web_token,
                Accept = 'application/json') |> 
    req_url_query(page = .page,
                  status = '1|2|3|4|5|6',
                  cycle = '1|2') |> 
    req_cookie_preserve('slides/class_6/class_6_files/resp_comunas_cookies') |> 
    req_timeout(1) |> # Pausa para no recargar el servidor.
    req_options(ssl_verifypeer = FALSE)
  
  resp <- req_consulta |> 
    req_perform()
  
  resp <- resp |> 
    resp_body_json()

  resp$comunas
}

# Leer las primeras dos páginas.
l_comunas <- map(1:2, 
                 json_page)

# Construcción de data.frame a partir de datos bajados
df_comunas <- l_comunas |> 
  reduce(bind_rows)

df_comunas |> 
  head(3)

df_comunas |> 
  write_excel_csv2('slides/class_6/class_6_taller/df_comunas_amigables.csv')


# Ejemplo 3: YouTube Data API ----
# 
# Construcción de llamados a APIs como de YouTube
# https://developers.google.com/youtube/v3/
# 
# Crear credenciales en 
# https://console.cloud.google.com/apis/credentials

f_date_for_api <- function(.date){
  .date |>
    with_tz(tzone = 'UCT') |> # Cambio de zona para pasar de hora chilena a UCT.
    format('%Y-%m-%dT%H:%M:%SZ')
}

## Búsqueda usando key ----
# 
# Documentación:
# https://developers.google.com/youtube/v3/docs/search

# Creo el url necesario para busqueda en YouTube

f_youtube_search <- function(q, ...) {
  request("https://youtube.googleapis.com/") |> 
    req_url_path("youtube/v3/search") |> 
    req_headers(Accept = 'application/json') |> 
    req_url_query(q = q,
                  publishedAfter = f_date_for_api(as.Date('2024-04-01')),
                  part = 'snippet',
                  maxResults = 15,
                  order = 'viewCount',
                  regionCode = 'CL',
                  ...)
}

req_youtube_search <- f_youtube_search(q = 'Encuestas en chile',
                                       key = Sys.getenv('YOUTUBE_KEY'))

req_youtube_search |> 
  req_dry_run()

# Envio la solicitud
resp_youtube_search <- req_youtube_search |> 
  req_perform()

resp_youtube_search

# Contenido de la respuesta como data.frame
df_search <- resp_youtube_search |> 
  resp_body_json(simplifyVector = TRUE)

df_search |> 
  str(2)

df_search$items |> 
  str(2, width = 120, strict.width = 'cut')

# Puedo extender la tabla para variables de id y snippet.
df_search_items_snippet <- df_search$items |> 
  unnest_wider(col = c(snippet, id),
               names_repair = 'unique')


# Contenido de la respuesta en formato json
json_search <- resp_youtube_search |> 
  resp_body_json(simplifyVector = FALSE)

json_search |> 
  str(2)

json_search$items |> 
  str(2)

# Transformar json a data.frame
df_search_items <- json_search$items |> 
  map(as.data.frame) |> 
  bind_rows() 

df_search_items |> glimpse()

df_search_items |> 
  select(id.videoId, snippet.publishedAt, snippet.title)


## Información de videos usando key ----
# 
# Documentación:
# https://developers.google.com/youtube/v3/docs/videos/

(l_videos <- df_search_items$id.videoId)

req_youtube_videos <- request("https://youtube.googleapis.com/") |> 
  req_url_path("youtube/v3/videos") |> 
  req_headers(Accept = 'application/json') |> 
  req_url_query(id = paste0(l_videos,
                            collapse = ','),
                part = paste0(c('snippet',
                                'contentDetails',
                                'statistics'),
                              collapse = ','),
                key = Sys.getenv('YOUTUBE_KEY'))

req_youtube_videos |> 
  req_dry_run()

resp_youtube_videos <- req_youtube_videos |> 
  req_perform()

resp_youtube_videos

# Contenido de la respuesta sobre videos como data.frame
df_videos <- resp_youtube_videos |>
  resp_body_json(simplifyVector = TRUE)

df_videos |> glimpse()

# Estadísticas
df_videos_stats <- df_videos$items |> 
  as_tibble() 

df_videos_stats |> 
  glimpse()

# Abrir las columnas de información para cada video
df_videos_stats <- df_videos_stats |> 
  unnest_wider(col = c(snippet,
                       contentDetails, 
                       statistics))

df_videos_stats |> 
  glimpse()

# Cambio en el tipo de variable de character a integer.
df_videos_stats <- df_videos_stats |> 
  mutate(across(c(viewCount, likeCount, favoriteCount, commentCount),
                as.integer))

df_videos_stats |> 
  ggplot(aes(y = str_trunc(title, 80) |> 
               str_wrap(50) |> 
               fct_reorder(viewCount),
             x = viewCount,
             fill = channelTitle)) +
  geom_col() +
  scale_x_continuous('Visualizaciones', 
                     labels = scales::number) +
  guides(fill = guide_legend(title = NULL,
                             keywidth = unit(3, 'mm'), 
                             keyheight = unit(5, 'mm'), 
                             nrow = 2)) +
  labs(title = 'Resultados busqueda YouTube',
       subtitle = "Término buscado: «Encuestas en Chile»",
       y = NULL) +
  theme(plot.title.position = 'plot',
        legend.position = 'top',
        legend.location = 'plot')


## Búsqueda usando OAuth v2.0 ----
# https://httr2.r-lib.org/articles/oauth.html
# 
# Tengo acceso a información de mi propio canal.
# 
# Direcciones web para autentificarse en Google.
httr::oauth_endpoints("google")

# httr provee otras direcciones de servicios habituales.
httr::oauth_endpoints("facebook")

# Configurar el flujo con OAuth 2.0

# Programa que va a recibir el acceso a YouTube con mis credenciales
f_client_youtube <- function(){
  oauth_client(
    id = "428304119447-rbtoartuj5cla185ubf0ridq5bun27ld.apps.googleusercontent.com",
    # Clave de acceso ofuscada
    secret = obfuscated("i9vWsEE91Gr4EU33FlkDupKjL56ZXSaI2bt-z8TRVmE5FQ3X9fDz2JCfBAqh6phiniwM"),
    token_url = "https://accounts.google.com/o/oauth2/token",
    name = "YouTube for scraping",
    
  )
}

f_client_youtube()

token_youtube <- oauth_flow_auth_code(
  client = f_client_youtube(), 
  auth_url = "https://accounts.google.com/o/oauth2/auth",
  scope = "https://www.googleapis.com/auth/youtube.force-ssl",
  redirect_uri = 'http://localhost:8080'
)

# Token para acceder a YouTube
token_youtube

# De esta forma es httr2 quien maneja los tokens.
req_youtube_search_oauth <- f_youtube_search(q = 'Encuestas en chile') |> 
  req_oauth_auth_code(client = f_client_youtube(), 
                      auth_url = "https://accounts.google.com/o/oauth2/auth",
                      scope = "https://www.googleapis.com/auth/youtube.force-ssl",
                      redirect_uri = 'http://localhost:8080')
  
resp_youtube_search_oauth <- req_youtube_search_oauth |> 
  req_perform()

df_youtube_search_oauth <- resp_youtube_search_oauth |> 
  resp_body_json(simplifyVector = TRUE)

df_youtube_search_oauth$items |> 
  str(2, width = 120, strict.width = 'cut')

# Puedo extender la tabla para variables de id y snippet.
df_search_oauth_items_snippet <- df_youtube_search_oauth$items |> 
  unnest_wider(col = c(snippet, id),
               names_repair = 'unique')

df_search_oauth_items_snippet |> 
  select(videoId, publishedAt, title)


### ANEXO: Usando httr -----

oauth_flow <- httr::oauth_app(
  appname = "YouTube for scraping",
  key = Sys.getenv("YOUTUBE_CLIENT_ID"),
  secret = Sys.getenv("YOUTUBE_CLIENT_SECRET")
)

# Datos para la autentificación
oauth_flow |> str()

# Direcciones web para autentificarse en Google.
httr::oauth_endpoints("google")

# httr provee otras direcciones de servicios habituales.
httr::oauth_endpoints("facebook")

# Pido el código de autorización para esta app.
youtube_token <- httr::oauth2.0_token(httr::oauth_endpoints("google"), 
                                      oauth_flow,
                                      scope = "https://www.googleapis.com/auth/youtube.force-ssl"
)

youtube_token

# Use the access token to access the YouTube Data API
response_youtube_oauth <- httr::GET("https://youtube.googleapis.com/youtube/v3/search",
                                    query = list(q = 'Encuestas en chile',
                                                 part = 'snippet',
                                                 maxResults = 25,
                                                 order = 'viewCount'),
                                    httr::config(token = youtube_token))