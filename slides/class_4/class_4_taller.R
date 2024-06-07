# title: "Lecturas de sitios web"
# subtitle: "Web scraping y acceso a datos desde la web"
# author: Cristián Ayala
# date: 2024-06-06

suppressPackageStartupMessages(library(tidyverse))
library(rvest)

# Ejemplo 1: Biblioteca ----
# 
# Valores promedio por categoría

url <- 'http://books.toscrape.com/'


## Web main ----
html_main <- read_html(paste0(url, 'index.html'))


## data.frame con categorías ----
l_cat <- html_main |> 
  html_elements('ul.nav ul a')

df_cat <- tibble(categoria = l_cat |> html_text(),
                 link      = l_cat |> html_attr('href')) |> 
  mutate(categoria = stringr::str_squish(categoria))

# Lectura de página web inicial de cada categoría 
df_cat <- df_cat |> 
  rowwise() |> 
  mutate(pagina = list(read_html(paste0(url, link))))

# Obtención de datos para construir links

html_main |>  
  html_elements('li.current') |> 
  html_text() |> 
  str_extract(' of (\\d{1,2})', group = 1)

f_n_paginas <- function(.html){
  # Función para obtener el número de páginas.
  x <- .html |> 
    html_elements('li.current') |> 
    html_text() |> 
    str_extract(' of (\\d{1,2})', group = 1)
  
  if(identical(x, character(0))) {
    x <- "0"
  }
  as.integer(x)
}

df_cat <- df_cat |> 
  mutate(libros_n = html_elements(pagina, '.form-horizontal strong:first-of-type') |> 
           html_text(),
         paginas_n = f_n_paginas(pagina))

# Construcción de links de páginas
# 
# Los links tienen la siguiente forma:
# http://books.toscrape.com/catalogue/category/books/mystery_3/page-1.html

f_make_page <- function(.n){
  # construcción de vector de page-# según el número capturado.
  text <- 'index.html'
  
  # Crea link para página si hay más de 1
  if(.n > 0){
    text <- paste0(rep('page-', .n), seq_len(.n), '.html')
    text[1] <- 'index.html'
  }
  text
}

# Ejemplo:
f_make_page(0)
f_make_page(3)

df_cat <- df_cat |> 
  mutate(paginas_link = list(f_make_page(paginas_n)))

head(df_cat, 3)

# Veamos los nombres de las páginas web creadas para las 3 primeras categorías:
df_cat$paginas_link |> 
  head(3)


# Agregar el root del link para cada página

df_cat <- df_cat |> 
  ungroup() |> 
  mutate(link = str_remove(link, 'index.html$')) |> 
  rowwise() |> 
  mutate(paginas_link = list(paste0(link, paginas_link)))

# Examinemos el link completo:
df_cat$paginas_link |> 
  head(3)

# Base de datos extendida: cada fila es una página.
df_cat_hojas <- df_cat |> 
  unnest_longer(paginas_link)

head(df_cat_hojas, 3)

# Dividir base para evitar duplicar trabajo

df_cat_hojas <- df_cat_hojas |> 
  mutate(ya_leido = str_detect(paginas_link, 'index.html$'))

df_cat_hojas_ya_leidas <- df_cat_hojas |> 
  filter(ya_leido)

df_cat_hojas_por_leer <- df_cat_hojas |> 
  filter(!ya_leido)

# Lectura de páginas por leer.
df_cat_hojas_por_leer <- df_cat_hojas_por_leer |> 
  rowwise() |> 
  mutate(pagina = list(read_html(paste0(url, paginas_link))))

# Unión de páginas leidas y por leer en una tabla.
df_cat_hojas_leido <- bind_rows(df_cat_hojas_ya_leidas,
                                df_cat_hojas_por_leer) |> 
  select(categoria, libros_n, pagina)

df_cat_hojas_leido |> head(3)

# Nombre de los libros
df_cat_hojas_leido$pagina[[1]] |> 
  html_elements('h3 a')

# Precio
df_cat_hojas_leido$pagina[[1]] |> 
  html_elements('.price_color') |> 
  html_text()

f_datos_libros <- function(.html){
  tibble(libro_name  = .html |> html_elements('h3 a') |> 
           html_attr('title'),
         libro_price = .html |> html_elements('.price_color') |> 
           html_text(),
         libro_url   = .html |> html_elements('h3 a') |> 
           html_attr('href'))
}

df_cat_hojas_leido$pagina[[1]] |> f_datos_libros()

# Extraigo información para cada hoja. Quedará como una data.frame.
df_cat_hojas_leido <- df_cat_hojas_leido |> 
  rowwise() |> 
  mutate(libros = list(f_datos_libros(pagina)))

head(df_cat_hojas_leido, 3)

# Una fila por libro.
df_cat_hojas_leido <- df_cat_hojas_leido |> 
  unnest(libros)

head(df_cat_hojas_leido, 3)

# Limpio información de precio.
df_cat_hojas_leido <- df_cat_hojas_leido |> 
  mutate(libro_price = str_remove(libro_price, '£'),
         libro_price = as.double(libro_price))


## Resultado ----

df_cat_hojas_leido |> 
  summarise(libros_n = n(),
            libros_price = mean(libro_price),
            libros_price_min = min(libro_price),
            libros_price_max = max(libro_price),
            .by = categoria)


# Ejemplo 2: Hockey Teams ----

url <- 'https://www.scrapethissite.com'

## Formulario ----
# 
# Usaremos el formulario para hacer búsqueda de los equipos de New York
s_main <- session(url)

# Navego a la página con el formulario.
s_main <- s_main |> 
  session_jump_to(url = 'pages/forms')

(f_search <- html_form(s_main))
f_search <- f_search[[1]] # Pueden haber más de un formulario.


### html Buscamos por New York y obtenemos el html de respuesta. ----
f_search <- f_search |> 
  html_form_set(q = "New York")

# Quedó registrada la palabra de búsqueda para el token q.
f_search

resp <- html_form_submit(f_search)

# Primera página de respuestas. 
page_1 <- read_html(resp) |> 
  html_table()

page_1

### session Buscamos por New York y modificamos la sesión de respuesta. ----
s_resp <- session_submit(s_main,
                         form = f_search)

df_page_1s <- read_html(s_resp) |> 
  html_element('table') |> 
  html_table()

# Primera pagina capturada
df_page_1s

# Obtengamos los links restantes:
links_pagination <- s_resp$response |> 
  httr::content() |> # Extrae el contenido de una respuesta.
  html_elements('.pagination a') |> 
  html_attr('href')

# Dejo solo los links de interés: elimino el 1º y el último.
links_pagination <- links_pagination[c(-1, -length(links_pagination))]

# Leo todos los links y luego obtengo una tabla.
page_restantes <- URLencode(links_pagination) |> # URL Encode por los " " (espacios)
  map(\(x) session_jump_to(s_resp, url = x))

l_page_2s <- page_restantes |> 
  # map porque podrían ser varias páginas restantes.
  map(\(x) read_html(x) |> 
        html_table())

# Estructura de lo capturado
l_page_2s |> 
  str()

# Veamos el número de niveles de profundidad.
l_page_2s |> 
  pluck_depth()

# Para obtener a una data.frame, es necesario quitar dos niveles.
df_page_2s <- l_page_2s |> 
  map(\(x) pluck(x, 1)) |> 
  _[[1]]

df_page_2s |> 
  head()


# Tabla completa

bind_rows(df_page_1s,
          df_page_2s)


### Creación de links si se conoce su formato ----

# Capturar 2a página buscando New York. Igual que lo hecho anteriormente.
page_2 <- s_resp |> 
  session_jump_to('?page_num=2&q=New%20York')

page_2 |> 
  read_html() |> 
  html_table()

# Capturar 1a página buscando Philadelphia.
page_phi_1 <- s_resp |> 
  session_jump_to('?page_num=1&q=Philadelphia')

page_phi_1 |> 
  read_html() |> 
  html_table()



## Analizando la url de búsqueda ----
# 
# 'https://www.scrapethissite.com/pages/forms/?page_num=1&per_page=25&q=New%20York'

url <- 'https://www.scrapethissite.com/pages/forms/?per_page=100&q=New%20York'

resp <- read_html(url)

# Puedo obtener todos los datos para New York.
resp |> 
  html_table()


# Obteniendo todos los datos de la página.
url <- 'https://www.scrapethissite.com/pages/forms/?per_page=1000'

resp <- read_html(url)

df_completa <- resp |> 
  html_table() |> 
  _[[1]]

df_completa |> nrow()


# Ejemplo 3: Oscar Winning Films: AJAX and Javascript ----
# 
# En la página se tiene que ver la petición de datos para obtener la información.

url <- 'https://www.scrapethissite.com/pages/ajax-javascript/'

read_html(url) |> 
  html_element('body') |> 
  html_text()

# Cuando se actualizan los años, esta es la dirección utilizada
url_ajax <- 'https://www.scrapethissite.com/pages/ajax-javascript'


# Año 2010
jsonlite::fromJSON(paste0(url_ajax, '?ajax=true&year=2010'))

# Lectura de todos los años

anios <- 2010:2015
url_anios <- paste0(url_ajax, '?ajax=true&year=', anios)


l_ajax <- map(url_anios,
              jsonlite::fromJSON)

str(l_ajax, 1)

# Crear data.frame a partir de la lista
df_ajax <- l_ajax |> 
  list_rbind()

df_ajax |> 
  head()

# Películas por año.
df_ajax |> count(year)

# Filas y columnas de base.
df_ajax |> 
  dim()


# Se puede crear una sesión usando httr2 (https://httr2.r-lib.org):

s_ajax <- httr2::request(url_ajax) |> 
  httr2::req_url_query(ajax = 'true', 
                       year = 2010)

s_ajax

s_ajax |> 
  httr2::req_dry_run()

# Capturo información desde Internet.
s_ajax_resp <- s_ajax |> 
  httr2::req_perform()

s_ajax_resp

s_ajax_resp |> 
  httr2::resp_body_json() |> 
  str(2)


# Ejemplo 4: SPD ----
# 
# Listado de estudios y archivos

url <- 'http://cead.spd.gov.cl/centro-de-documentacion/'

html <- read_html(url)

html |> 
  html_elements('body')

# Tabla con estudios.
url_tabla <- 'http://cead.spd.gov.cl/wp-content/plugins/download-manager/tpls/query.php?length=-1&selectTotal=-1'

spd_proj <- jsonlite::fromJSON(url_tabla)

spd_proj |> 
  str()

df_spd_proj <- spd_proj |> 
  as_tibble()

# La columna aaData es una matriz
head(df_spd_proj)

head(df_spd_proj$aaData, 2)

# De matriz a data.frame y luego unpack
df_spd_proj <- df_spd_proj |> 
  mutate(aaData = as.data.frame(aaData)) |> 
  unpack(aaData)

head(df_spd_proj)

# Links para descargar archivos. 
# Probemos con el 1er caso.
df_spd_proj$V5[[1]] |> 
  xml2::as_xml_document() |> 
  html_element('a') |>
  html_attr('href')

# Función para obtener el link de descarga de cada archivo.
f_links_spd <- function(.chr){
  xml2::as_xml_document(.chr) |> 
    html_element('a') |>
    html_attr('href')
}

# Extraigo el link para todas las filas.
df_spd_proj <- df_spd_proj |> 
  rowwise() |> 
  mutate(link = f_links_spd(V5),
         .before = V5) |> 
  ungroup()

# Construyo el link completo, agregando la base.
df_spd_proj <- df_spd_proj |> 
  mutate(link_full = paste0('http://cead.spd.gov.cl', link),
         .after = link)

s_ajax <- httr2::request(url_ajax) |> 
  httr2::req_url_query(ajax = 'true', 
                       year = 2010)


### Bajar archivos ----
# Prueba con un link
req_file <- httr2::request(df_spd_proj$link_full[[1]])
req_file |> httr2::req_dry_run()

file <- req_file |> httr2::req_perform()

# Obtener el nombre del archivo desde el header del requerimiento.
httr2::resp_headers(file)

# El título está en el header "Content-disposition".
file_name <- httr2::resp_header(file, "Content-disposition")

# Debo limpiarlo
file_name <- file_name |> 
  str_extract('.\"(.*)\".*', group = 1)

# Guardar el archivo como binario.
writeBin(httr2::resp_body_raw(file),
         con = file.path('slides/class_4/class_4_taller_download/', 
                         file_name))


# Usaremos solo 2 archivos para no bajar demasiada información

# Función para capturar la información
f_get_file <- function(url){
  httr2::request(url) |> 
    httr2::req_perform()
}

# Función para extraer el nombre del archivo capturado.
f_get_file_name <- function(x){
  httr2::resp_header(x, "Content-disposition") |> 
    str_extract('.\"(.*)\".*', group = 1)
}

# Leo dos archivos.
df_spd_proj_test <- df_spd_proj |> 
  slice(2:3) |> 
  rowwise() |> 
  mutate(file = list(f_get_file(link_full)),
         file_name = f_get_file_name(file),
         .after = V1)

df_spd_proj_test

# Guardo los archivos.
walk2(df_spd_proj_test$file,
      df_spd_proj_test$file_name,
      \(f, fn) writeBin(httr2::resp_body_raw(f),
                        con = file.path('slides/class_4/class_4_taller_download/', 
                                        fn)))
