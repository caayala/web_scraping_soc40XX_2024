## -----------------------------------------------------------------------------
#| echo: false
#| warning: false
suppressPackageStartupMessages(library(tidyverse))
library(rvest)
library(httr2)
library(knitr)

opts_chunk$set(cache.path = "class_10_files/class_10_cache/html/")


## -----------------------------------------------------------------------------
url <- 'https://admision.mineduc.cl/vitrina-vue/'

read_html(url) |> 
  html_element('body')


## -----------------------------------------------------------------------------
url_imdb <- 'https://www.imdb.com/search/title/?title_type=feature&locations=chile&countries=CL'
imbd_static <- read_html(url_imdb)
imbd_live   <- read_html_live(url_imdb)


## -----------------------------------------------------------------------------
imbd_static |> html_elements('.cVXqoq') |> length()


## -----------------------------------------------------------------------------
imbd_live |> html_elements('.cVXqoq') |> length()


## -----------------------------------------------------------------------------
n_peliculas <- imbd_live |> html_element('.gJQFCa') |> html_text()
(n_peliculas <- n_peliculas |> str_extract('.*?(\\d*)$', group = 1) |> as.integer())


## -----------------------------------------------------------------------------
(n_clics <- ceiling(n_peliculas / 50) - 1)


## -----------------------------------------------------------------------------
imbd_live$view()


## -----------------------------------------------------------------------------
imbd_live$click('.ipc-see-more__text')


## -----------------------------------------------------------------------------
f_clic <- function(...){
  imbd_live$click('.ipc-see-more__text')
  Sys.sleep(2) # Pausa de 2 segundos entre clics.
}

walk(seq_len(n_clics - 2), f_clic)


## -----------------------------------------------------------------------------
imbd_live |> html_elements('.cVXqoq') |> length()


## -----------------------------------------------------------------------------
# Ir a sitio web de interés
try(vue_live <- read_html_live("https://admision.mineduc.cl/vitrina-vue/"))
Sys.sleep(5) # Tiempo para que la página cargue.


## -----------------------------------------------------------------------------
# Se puede ver lo que hace esta "ventana":
if(interactive()){
  vue_live$view()
}


## -----------------------------------------------------------------------------
try(select_reg  <- vue_live |> html_elements("select[id=select-region]"))
try(select_com  <- vue_live |> html_elements("select[id=select-comuna]"))
try(opt_reg_ids <- vue_live |> html_elements("select[id=select-region] option"))

## -----------------------------------------------------------------------------
try(opt_reg_ids |> html_text())


## -----------------------------------------------------------------------------
try(
  df_region <- tibble(region = opt_reg_ids |> html_text(),
                      value = opt_reg_ids |> html_attr('value') |> as.integer())
)

df_region |> head()


## -----------------------------------------------------------------------------
comuna <- 'ALTO_HOSPICIO'

req_esc <- request('https://apisae.mineduc.cl/sae-api-vitrina/v1/establecimientos') |> 
  req_url_query(comuna = comuna) |> 
  req_headers('accept' = 'application/json',
              'accept-encoding' = 'gzip')

(resp_esc <- req_esc |> req_perform())


## -----------------------------------------------------------------------------
resp_esc |> 
  resp_body_json(simplifyVector = TRUE) |> head()


## -----------------------------------------------------------------------------
f_comuna_escuelas <- function(.comuna){
  # preparación del envío
  req_esc <- request('https://apisae.mineduc.cl/sae-api-vitrina/v1/establecimientos') |> 
    req_url_query(comuna = .comuna) |> 
    req_headers('accept' = 'application/json',
                'accept-encoding' = 'gzip')
  
  req_esc |> req_perform() |> 
    resp_body_json(simplifyVector = T)
}


## -----------------------------------------------------------------------------
js_comunas <- read_html('https://admision.mineduc.cl/vitrina-vue/static/js/app.08fc6ac089af64dc7303.js')

comunas <- js_comunas |> html_text() |> 
  str_extract_all('nombreComuna:\"(.*?)\"')

comunas[[1]] |> head()


## -----------------------------------------------------------------------------
comunas <- comunas[[1]] |> 
  str_remove_all(r'(nombreComuna:\"|\")') |>  # R 4.0 soporta "raw strings"
  janitor::make_clean_names(case = 'all_caps') # "_" por " " y eliminar caracteres especiales (Ñ)

comunas |> head()


## -----------------------------------------------------------------------------
comunas[1:20]


## -----------------------------------------------------------------------------
df <- f_comuna_escuelas(comunas[1])

head(df)


## -----------------------------------------------------------------------------
l_escuelas <- map(comunas[1:4], 
                  f_comuna_escuelas)

df_escuelas <- l_escuelas |> 
  list_rbind()

df_escuelas |> head()


## -----------------------------------------------------------------------------
#| echo: true
f_rbd_detalle <- function(.rbd){
  req_rbd <- request('https://apisae.mineduc.cl/sae-api-vitrina/v1/establecimientos') |> 
    req_url_path_append(.rbd)
  
  req_rbd |> req_perform() |> 
    resp_body_json(simplifyVector = TRUE)
}

df_rbd_detalle <- f_rbd_detalle(
  df_escuelas$rbd[[1]]
  )


## -----------------------------------------------------------------------------
#| echo: true
df_rbd_detalle |> str(1)


## -----------------------------------------------------------------------------
#| cache = TRUE
l_rbd_detalle <- map(df_escuelas$rbd[1:3], 
                     f_rbd_detalle)

df_rbd_detalle <- l_rbd_detalle |>
  enframe() |> 
  unnest_wider(value)

head(df_rbd_detalle)


## -----------------------------------------------------------------------------
library(selenider)

# A blog de desuc
# if(interactive()){
  open_url("https://blog.desuc.cl")
# }


## -----------------------------------------------------------------------------
link_entrada_1 <- s('.post-preview') |> 
  elem_attr('href')

link_entrada_1


## -----------------------------------------------------------------------------
open_url(paste0(current_url(), link_entrada_1))

s('p:nth-child(4)')


## -----------------------------------------------------------------------------
back()

ss('.post-preview h2') |> 
  lapply(elem_text) |> 
  head()


## -----------------------------------------------------------------------------
#| echo = FALSE,
#| include = FALSE

# Extraer código R
knitr::purl('class_10.qmd',
            output = 'class_10.R',
            quiet = TRUE)

