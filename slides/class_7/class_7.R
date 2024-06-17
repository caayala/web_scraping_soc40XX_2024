## -----------------------------------------------------------------------------
#| echo = FALSE
suppressPackageStartupMessages(library(tidyverse))
library(labelled)
library(knitr)

opts_chunk$set(cache.path = "class_7_files/class_7_cache/html/")


## -----------------------------------------------------------------------------
#| message: true
gargle::gargle_oauth_sitrep()


## -----------------------------------------------------------------------------
httr2::url_parse('https://docs.google.com/spreadsheets/d/1rujigrFqSaSLjsSvrMZY4IU3CSLiM_ymd-wtst9BYNQ/edit#gid=398551276') |> 
  discard(is.null)


## -----------------------------------------------------------------------------
library(googledrive)
# Elección de cuenta si es que se tienen varias
options(gargle_oauth_email = "caayala@uc.cl")


## -----------------------------------------------------------------------------
# Solo spreadsheet
# Al acceder al servicio por primera vez, debiese preguntar por las credenciales.
(ss_b <- googledrive::drive_find('Web Scraping', 
                                 type = 'spreadsheet'))


## -----------------------------------------------------------------------------
ss_b$drive_resource |> str(1)


## -----------------------------------------------------------------------------
ss_b$drive_resource[[1]] |> names()


## -----------------------------------------------------------------------------
# Solo archivos pdf
googledrive::drive_find('Cuestionario Bicentenario', type = 'pdf')


## -----------------------------------------------------------------------------
library(googlesheets4)


## -----------------------------------------------------------------------------
class(ss_b)


## -----------------------------------------------------------------------------
# Al acceder al servicio por primera vez, debiese preguntar por las credenciales.
df_1 <- read_sheet(ss = ss_b,
                   sheet = 'bicentenario_2020.csv')
head(df_1)


## -----------------------------------------------------------------------------
url_sheet <- 'https://docs.google.com/spreadsheets/d/1rujigrFqSaSLjsSvrMZY4IU3CSLiM_ymd-wtst9BYNQ/'

df_2 <- read_sheet(ss = url_sheet,
                   sheet = 'bicentenario_2020.csv')


## -----------------------------------------------------------------------------
head(df_2)


## -----------------------------------------------------------------------------
ss <- '1rujigrFqSaSLjsSvrMZY4IU3CSLiM_ymd-wtst9BYNQ'

df_3 <- read_sheet(ss = ss,
                   sheet = 'bicentenario_2020.csv')


## -----------------------------------------------------------------------------
head(df_3)


## -----------------------------------------------------------------------------
gs4_get(ss)


## -----------------------------------------------------------------------------
sheet_properties(ss)


## ----message=FALSE------------------------------------------------------------
df_base <- read_sheet(ss, sheet = 'bicentenario_2020.csv')
df_niv  <- read_sheet(ss, sheet = 'niveles')
df_var  <- read_sheet(ss, sheet = 'variables')


## ----message=FALSE------------------------------------------------------------
head(df_var, 4)


## ----message=FALSE------------------------------------------------------------
df_var  <- read_sheet(ss, sheet = 'variables',
                      col_types = 'c') # tipo de variable se recicla.

head(df_var, 4)


## -----------------------------------------------------------------------------
df_cels <- range_read_cells(ss, sheet = 'bicentenario_2020.csv', 
                            cell_data = 'default')

head(df_cels, 3)


## -----------------------------------------------------------------------------
df_cels[1, ] |> str()


## -----------------------------------------------------------------------------
df_cels$cell[[1]]


## -----------------------------------------------------------------------------
df_cels |> 
  rowwise() |> 
  mutate(formattedValue = pluck(cell, 'formattedValue'))


## -----------------------------------------------------------------------------
df_cels_full <- range_read_cells(ss, sheet = 'bicentenario_2020.csv', 
                                 cell_data = 'full')

head(df_cels_full)


## -----------------------------------------------------------------------------
df_cels_full[1, ]$cell |> str(3)


## -----------------------------------------------------------------------------
df_cels_folio_color <- df_cels_full |> 
  filter(col == 1) |>  # Folio está en columna 1
  rowwise() |> 
  mutate(formattedValue = pluck(cell, 'formattedValue') |> 
           str_trunc(width = 15),
         backgroundColorStyle = list(
           pluck(cell, 'effectiveFormat', 'backgroundColor')
           ))

# Sencillo revisar los colores como hex: 
df_cels_folio_color$hex <- map_chr(df_cels_folio_color$backgroundColorStyle, 
                                   ~rlang::inject(rgb(!!!.)))

table(df_cels_folio_color$hex)


## -----------------------------------------------------------------------------
df_cels_folio_color |> 
  filter(hex == '#F4CCCC') |> 
  pull(formattedValue)


## -----------------------------------------------------------------------------
df_var_orden <- df_var[match(names(df_base), df_var$bicentenario_2020.csv), ]

names(df_base) <- df_var_orden$variables

head(df_base, 2)


## -----------------------------------------------------------------------------
labelled::var_label(df_base) <- df_var_orden$bicentenario_2020.csv


## -----------------------------------------------------------------------------
df_niveles <- df_niv |> 
  tidyr::nest(.by = tipo,
              .key = 'niveles') |> 
  rowwise() |> 
  mutate(niveles = list(deframe(rev(niveles))))

df_niveles


## -----------------------------------------------------------------------------
df_niveles |> str(3)


## -----------------------------------------------------------------------------
v_values <- df_var_orden |> 
  filter(!is.na(tipo)) |> 
  pull(tipo, variables)

v_values


## -----------------------------------------------------------------------------
value_list <- structure(df_niveles$niveles[match(as.character(v_values), 
                                                 df_niveles$tipo)],
                        names = names(v_values))
value_list


## -----------------------------------------------------------------------------
df_base <- df_base |> 
  labelled::set_value_labels(!!!value_list)

df_base |> 
  head()


## -----------------------------------------------------------------------------
df_base_fct <- df_base |> 
  mutate(across(everything(), as_factor))

df_base_fct |> 
  head(3)


## -----------------------------------------------------------------------------
f_tabla <- function(.df, .cat, .var){
  .df |> 
    count({{ .cat }}, {{ .var }}) |> 
    pivot_wider(names_from = {{ .cat }}, 
                values_from = n)  
}

df_tabla <- map(rlang::exprs(sexo, tramo_edad), 
                \(x) f_tabla(df_base_fct, !!x, s1_1))

df_tabla


## -----------------------------------------------------------------------------
df_tabla <- df_tabla |> 
  reduce(left_join, by = 's1_1')

df_tabla


## -----------------------------------------------------------------------------
#| message: false

# Reemplaza formatos en la hoja de destino
df_tabla |> 
  sheet_write(ss,
              sheet = 'resultados')


## -----------------------------------------------------------------------------
#| message: false

range_clear(ss, sheet = 'resultados', reformat = TRUE)


## -----------------------------------------------------------------------------
# NO reemplaza formatos en la hoja de destino
range_write(ss,
            df_tabla,
            sheet = 'resultados',
            reformat = FALSE)


## -----------------------------------------------------------------------------
#| include: false

# Extraer código R
knitr::purl('class_7.qmd',
            output = 'class_7.R',
            quiet = TRUE)

