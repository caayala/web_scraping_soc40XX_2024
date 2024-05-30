## -----------------------------------------------------------------------------
suppressPackageStartupMessages(library(tidyverse))
library(rvest)
url <- 'class_1_files/mi_primer_scraping.html'

if(interactive()){
  url <- 'slides/class_1/class_1_files/mi_primer_scraping.html'
}

html <- read_html(x = url)
html


## -----------------------------------------------------------------------------
html |> html_element('body')


## -----------------------------------------------------------------------------
html |> html_element('p')


## -----------------------------------------------------------------------------
html |> html_element('#first') |> html_text()


## -----------------------------------------------------------------------------
html |> html_element('img') |> html_attr('src')


## -----------------------------------------------------------------------------
html |> html_element('img') |> html_attrs()


## -----------------------------------------------------------------------------
html |> html_element('body') |> html_children() |> html_name()


## -----------------------------------------------------------------------------
#| cache = TRUE
url <- 'https://es.wikipedia.org/wiki/Star_Wars'
html <- read_html(url) # Leo la página web

df_tablas <- html |> 
  html_elements('table.wikitable') |>  html_table()

df_tablas |> str(1)


## -----------------------------------------------------------------------------
df_recaudacion <- df_tablas[[8]]
df_recaudacion |> head(3) # Recaudación


## -----------------------------------------------------------------------------
df_critica <- df_tablas[[9]]
df_critica |> head(3) # Evaluación


## -----------------------------------------------------------------------------
# Corrección de nombre de columnas
colnames(df_recaudacion) <- as.character(df_recaudacion[1, ])
  
df_recaudacion <- df_recaudacion |> 
  filter(str_detect(Película, 'Star Wars')) |> # Star Wars
  mutate(across(3:6, \(x) str_remove_all(x, '\\.|\\$')), # Eliminar "." y "$" en números
         across(3:6, as.integer))


## -----------------------------------------------------------------------------
#| echo = FALSE
df_recaudacion$Película <- df_recaudacion$Película |> str_remove('Star Wars: ')
df_recaudacion$Película <- coalesce(
  df_recaudacion$Película |> str_extract('.* - (.*)\\[.*', group = 1),
  df_recaudacion$Película |> str_extract('^(.*): .*', group = 1)
)

df_recaudacion$Película[df_recaudacion$Película == 'The Empire Strikes Back'] <- 'Empire Strikes Back'
df_recaudacion$Película[df_recaudacion$Película == 'Rise of Skywalker']       <- 'The Rise of Skywalker'

df_recaudacion # Dejo fuera "The Clone Wars"


## -----------------------------------------------------------------------------
colnames(df_critica) <- as.character(df_critica[1, ])
  
df_critica <- df_critica |> 
  filter(!(Película %in% c('Película', 'Promedio'))) |> 
  mutate(across(2:7, \(x) str_extract(x, '\\d.?\\d')), # capturo las notas
         across(6:7, \(x) str_replace(x, '\\,', '\\.')), # reemplazo ',' por '.'
         across(2:5, as.integer),
         across(6:7, as.double)) # transformo strings a números.


## -----------------------------------------------------------------------------
df_critica


## -----------------------------------------------------------------------------
#| fig.dim=c(7, 4)
df_cyr <- left_join(df_critica, df_recaudacion, by = 'Película')

ggplot(df_cyr,
       aes(x = Total, y = General)) + 
  geom_point(size = rel(3)) +
  ggrepel::geom_label_repel(aes(label = Película)) +
  scale_x_continuous('Millones de dólares', labels = ~scales::dollar(., scale = 0.000001)) + 
  labs(title = 'Star Wars: Relación entre recaudación total y crítica (Rotten Tomatoes)') +
  theme_minimal()


## -----------------------------------------------------------------------------
#| echo = FALSE,
#| include = FALSE

# Extraer código R
knitr::purl('class_1.qmd',
            output = 'class_1.R',
            quiet = TRUE)

