## -----------------------------------------------------------------------------
#| echo = FALSE
suppressPackageStartupMessages(library(tidyverse))
library(rvest)
library(knitr)

opts_chunk$set(cache.path = "class_3_files/class_3_cache/html/")


## -----------------------------------------------------------------------------
#| echo: false

if(interactive()){
  url <- 'class_3_files/scraping_con_css.html'
} else {
  url <- 'class_3_files/scraping_con_css.html'
}

page <- htmltools::includeHTML(url) # Guardar el texto del html de la url.

page |> gsub('<!DOCTYPE html>\n', '', x = _)


## -----------------------------------------------------------------------------
#| echo: true
# read_html(page)
(html1 <- rvest::read_html(x = url)) # Leer la url directamente.


## -----------------------------------------------------------------------------
html1 |> html_elements('table') |> html_table()


## -----------------------------------------------------------------------------
(df_img_perros <- html1 |> 
   html_element('#tabla-imagen-perros') |> 
   html_table())


## -----------------------------------------------------------------------------
(src_img_perros <- html1 |> html_elements('#tabla-imagen-perros') |> 
   html_elements('img') |> 
   html_attr('src'))


## -----------------------------------------------------------------------------
# Uso rbind porque tiene menos salvaguardas que `bind_rows`.
(df_img_perros <- base::rbind(df_img_perros,
                              src_img_perros))


## -----------------------------------------------------------------------------
df_img_perros |> 
  t() |> as.data.frame() |>
  setNames(nm = c('raza', 'fuente', 'img_src'))


## -----------------------------------------------------------------------------
html1 |> html_element('#first') |> html_text()


## -----------------------------------------------------------------------------
html1 |> html_elements('p') |> html_text()


## -----------------------------------------------------------------------------
html1 |> html_elements('p b') |> html_text()


## -----------------------------------------------------------------------------
html1 |> html_elements('a[href]')


## -----------------------------------------------------------------------------
html1 |> html_elements('[alt*=akita]')


## -----------------------------------------------------------------------------
#| echo: true

url <- 'http://books.toscrape.com/'
html2 <- read_html(paste0(url, 'index.html'))


## -----------------------------------------------------------------------------
l_cat <- html2 |> 
  html_elements('ul.nav ul a')

head(l_cat, 2)


## -----------------------------------------------------------------------------
#| echo: true

df_cat <- tibble(categoria = l_cat |> html_text(),
                 link      = l_cat |> html_attr('href')) |> 
  mutate(categoria = stringr::str_squish(categoria))

head(df_cat, 2)


## -----------------------------------------------------------------------------
df_cat_hojas <- df_cat |> 
  mutate(pagina = map(link, \(l) read_html(paste0(url, l))))

head(df_cat_hojas, 3)


## -----------------------------------------------------------------------------
df_cat[1:3, ] |>
  dplyr::rowwise() |>
  mutate(pagina = list(read_html(paste0(url, link))))


## -----------------------------------------------------------------------------
# Creo una función para reutilizar el código más adelante.

f_n_libros <- function(.html){
  html_elements(.html, '.form-horizontal strong:first-of-type') |> html_text()
}


## -----------------------------------------------------------------------------
html2 |> f_n_libros()


## -----------------------------------------------------------------------------
df_cat_hojas <- df_cat_hojas |> 
  mutate(n_libros = map_chr(pagina, f_n_libros),
         n_libros = as.integer(n_libros))

df_cat_hojas |> head(3)


## -----------------------------------------------------------------------------
table(df_cat_hojas$n_libros, useNA = 'ifany') |> sort()


## -----------------------------------------------------------------------------
df_cat_hojas |> 
  rowwise() |> 
  mutate(n_libros = f_n_libros(pagina),
         n_libros = as.integer(n_libros)) |> 
  head(3)


## -----------------------------------------------------------------------------
#| fig-dim: c(14, 4)

df_cat_hojas |> 
  ggplot(aes(x = fct_reorder(categoria, -n_libros), y = n_libros)) + 
  geom_col() + 
  scale_x_discrete(NULL, guide = guide_axis(angle = 90)) +
  labs(title = 'Número de libros por categoría') + theme_minimal()


## -----------------------------------------------------------------------------
#| echo = FALSE,
#| include = FALSE

# Extraer código R
knitr::purl('class_3.qmd',
            output = 'class_3.R',
            quiet = TRUE)

