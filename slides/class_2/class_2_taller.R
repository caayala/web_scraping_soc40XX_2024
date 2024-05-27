# title: "Trabajo con listas"
# subtitle: "Web scraping y acceso a datos desde la web con R"
# author: Cristián Ayala
# date: 2024-05-30

suppressPackageStartupMessages(library(tidyverse))
library(rvest)

# purrr Fundamentos ----

l <-  list(a = c('caso' = c(1:2, NA)),
           b = c('caso' = 4:5))
l

# Estructura del objeto.
str(l)

# Suma de cada elemento de la lista.
l |> purrr::map(sum)
l |> purrr::map(function(x) sum(x))

# Distintas formas de anotar la función o qué se hará con cada elemento.
l |> purrr::map(sum, na.rm = TRUE) # No se recomienda usar ... para pasar nuevos argumentos.
l |> purrr::map(~sum(., na.rm = TRUE))
l |> purrr::map(function(x) sum(x, na.rm = TRUE))
l |> purrr::map(\(x) sum(x, na.rm = TRUE)) # Esta es la forma recomendada.

# Usando funciones de R base.
base::lapply(l, sum)
base::lapply(l, ~sum(., na.rm = TRUE)) # lapply no entiende este tipo de sintaxis.
base::lapply(l, \(x)sum(x, na.rm = TRUE))

# ¿Cuál es el largo de cada elemento?
l |> purrr::map(length)



# purrr Feriados en Chile ----

## Lectura de base de datos ----

url <- 'https://apis.digital.gob.cl/fl/feriados'

# De JSON a lista en R
l_feriados <- jsonlite::read_json(url, simplifyVector = FALSE)


## Explorar la lista ----

# ¿cuántos datos hay?
l_feriados |> length()

# ¿Cómo es su estructura?
l_feriados |> str()

# 1 nivel
l_feriados |> str(1)

# Los primeros 2 elementos de l_feriados para luego ver los 2 niveles.
l_feriados[1:2] |> str(2)

# Profundidad máxima de la lista. 
pluck_depth(l_feriados) # Tiene un máximo de 5 niveles.


## Extraer elementos ----

### `map` Nombres ----
# 
# ¿Cuáles son los nombres de los feriados?

l_feriados |> map('nombre') # Devuelve una lista

l_feriados |> map_chr('nombre') # Devuelve un vector de caracteres

l_feriados |> map_chr('nombre') |> table()

l_feriados |> map_chr('nombre') |> table() |> sort()


### `map` Fechas ----
# 
# ¿Cuáles son las fechas de los feriados? ¿Cuántos feriados fueron en fin de semana por año?

l_feriados |> map_chr('fecha') # Devuelve un vector de caracteres

# ¿Cuántos feriados fueron en fin de semana por año?

ch_fechas <- l_feriados |> map_chr('fecha') # Vector con fechas

# ¿Hay fechas duplicadas?
sum(duplicated(ch_fechas))

# ¿Cuáles son las fechas duplicadas?
ch_fechas[duplicated(ch_fechas)]

ch_fechas <- unique(ch_fechas) # solo fechas únicas

# Construcción de una tibble para tener datos ordenados.
df_fechas <- tibble(fecha = ch_fechas)

df_fechas <- df_fechas |> 
  mutate(fecha = as.Date(fecha))

# Quiero obtener el año y el día de la semana
# help(strptime)

df_fechas <- df_fechas |> 
  mutate(dia = format(fecha, '%u'), # Lunes es 1
         mes = format(fecha, '%m'),
         anio = format(fecha, '%Y'),
         fin_de_semana = dia > 5)

df_fechas |> head()

# Feriados por año
df_fechas$anio |> table()

# Graficar ----
library(ggtext)

# Ajustar el diseño de los gráficos.
theme_set(theme_minimal() + 
            # Posibilidad de uso de markdown en el título.
            theme(plot.title = element_markdown()))

# Número de feriados
gg_fer_num <- ggplot(df_fechas, 
                     aes(x = anio, 
                         fill = fin_de_semana)) +
  geom_bar() + 
  labs(title = 'Feriados por año: **número**',
       y = 'número')

# Visualizar el gráfico.
gg_fer_num

# Separar semanas de fin de semanas en facets.
gg_fer_num + facet_grid(rows = vars(fin_de_semana))

# Proporción de feriados
# Tabla por años.
df_fechas_prop <- df_fechas |> 
  count(anio, fin_de_semana) |> 
  mutate(prop = n / sum(n),
         .by = anio)
  
gg_fer_prop <- ggplot(df_fechas_prop, 
                      aes(x = anio, 
                          y = n,
                          fill = fin_de_semana)) +
  geom_col(position = position_fill()) +
  geom_text(aes(label = scales::percent(if_else(fin_de_semana, prop, NA),
                                        accuracy = 1)),
            vjust = 1,
            position = position_fill()) +

  labs(title = 'Feriados por año: **proporción**',
       y = 'proporción')

# Unir dos gráficos en uno solo.
library(patchwork)

gg_fer_num / gg_fer_prop +
  patchwork::plot_layout(guides = 'collect')

# ¿Cuál fue el mejor año?

### pluck Leyes ----
# 
# Ver si cada uno de los elementos tiene una lista de 'leyes'
l_feriados |> pluck(1, 'leyes') # "leyes" del elemento 1
l_feriados |> pluck(183, 'leyes')  # "leyes" del elemento 183

l_feriados[1:3] |> map(\(x) pluck(x, 'leyes'))
l_feriados[1:3] |> map(\(x) pluck(x, 'leyes')) |> map(\(x) pluck(x, 1, 'nombre'))

# El elemento 1 tiene dos leyes asociadas.
l_feriados[1:3] |> map(\(x) pluck(x, 'leyes', 1, 'nombre'))
l_feriados[1:3] |> map(\(x) pluck(x, 'leyes', 2, 'nombre'))


## Comentarios ----

# ¿Cómo extraemos los comentarios?
l_feriados |> map('comentarios')

# Error: No se puede generar un vector solamente con 'characters'.
l_feriados |> map_chr('comentarios')

l_feriados[[168]]['comentarios'] # elemento que genera error.
l_feriados[[167]]['comentarios']


# Revisión de todos los comentarios.
l_feriados |> map('comentarios') |> 
  as.character() |> 
  str_trunc(width = 40) |> 
  table(useNA = 'ifany') |> 
  sort()


# Cambio en el primer elemento el contenido de comentario
l_feriados |> 
  assign_in(list(1, 'comentarios'), 'cambio con assign_in') |> 
  _[[1]] |> # R 4.3
  str(1)

# Cambio todos los comentarios
l_feriados |> map(\(x) assign_in(x, list('comentarios'), 'cambio con assign_in'))


# Modifico comentarios para limpiarlos: 3 tipos de comentarios.
l_feriados[[144]]$comentarios #' '
l_feriados[[160]]$comentarios #''
l_feriados[[168]]$comentarios #NULL


f_blancos <- function(x){
  # Función para homologar los comentarios.
  if((x %in% c(' ', '', 'NULL')) || is.null(x)){
    NA_character_
  } else {
    x
  }
}

# Ejemplos de función
f_blancos('casa')
f_blancos(' ')
f_blancos(NULL)


### modify ----

l_feriados[[144]] |> modify_in('comentarios', f_blancos) |> str(1)
l_feriados[[168]] |> modify_in('comentarios', f_blancos) |> str(1)

# Modifiquemos todos los comentarios
l_feriados |> map(\(x) modify_in(x, 'comentarios', f_blancos))

# Formas equivalentes de anotar funciones anónimas "lambda".
l_feriados |> map(function(x) modify_in(x, 'comentarios', f_blancos))
l_feriados |> map(function(x){ modify_in(x, 'comentarios', f_blancos) })
l_feriados |> map(\(x) modify_in(x, 'comentarios', f_blancos))
l_feriados |> map(\(x){ modify_in(x, 'comentarios', f_blancos) })
l_feriados |> map(~modify_in(., 'comentarios', f_blancos))
l_feriados |> map(modify_in, 'comentarios', f_blancos)

# Modifico la lista
l_feriados <- l_feriados |> 
  map(\(x) modify_in(x, 'comentarios', f_blancos))


### keep ----
# Dejo los elementos en los que 'comentarios' no sean NA.
l_feriados |> 
  keep(\(x) !is.na(x['comentarios'])) |> 
  str(2)

### discard ----
# Descarto los elementos en los que 'comentarios' sea NA.
l_feriados |> 
  discard(\(x) is.na(x['comentarios'])) |> 
  str(2)

# Solo veo 'comentarios'.
l_feriados |> 
  discard(\(x) is.na(x['comentarios'])) |>
  map(pluck, 'comentarios')


## Lista a data.frame ----

# Pasar la lista a una tibble.
df_feriados <- l_feriados |> 
  enframe(name = 'id', value = 'feriados')

# Extiendo los datos en "feriados"
df_feriados |> 
  unnest_wider(feriados) # Vimos feriados con 2 o 3 leyes

# Alargo los datos en "leyes"
df_feriados |> 
  unnest_wider(feriados) |> 
  unnest_longer(leyes) 

df_feriados |> 
  unnest_wider(feriados) |> 
  unnest(leyes) |> 
  unnest_wider(leyes, 
               names_repair = 'minimal') # dos columnas de nombre "nombre"

# data.frame con los feriados. Las unidades de análisis
df_feriados <- df_feriados |> 
  unnest_wider(feriados) |> 
  unnest(leyes) |> 
  unnest_wider(leyes, names_repair = 'minimal')

# Se debe distinguir las dos variables con igual nombre: "nombre"
names(df_feriados)[7] <- 'nombre_ley'

# Dejar solo una fecha, evitando los duplicados. 
df_feriados <- df_feriados |> 
  distinct(across(id:tipo), .keep_all = TRUE) # Hasta tipo porque sino son datos únicos.


### fromJSON: Lectura directa de JSON a data.frame ----

# Podemos pedir a `read_json` que intente simplificar el JSON que recibe a un vector o data.frame.
jsonlite::read_json(url, simplifyVector = TRUE) |> 
  as_tibble()


# Equivalente:
df_feriados_2 <- jsonlite::fromJSON(url) |> 
  as_tibble()

df_feriados_2 |> 
  head()

df_feriados_2$comentarios |> 
  str_trunc(width = 40) |> table() |> sort()

# ¿Existe el problema con los comentarios igual que en la lista anterior?
df_feriados_2$comentarios |> 
  str_trunc(width = 40) |> table(useNA = 'ifany') |> sort()

## type_convert: Extra, ajustar la clase de las variables ----

df_feriados_2 |> 
  mutate(fecha = as.Date(fecha),
         irrenunciable = as.integer(irrenunciable))

df_feriados_2 <- df_feriados_2 |> 
  readr::type_convert()

df_feriados_2 |> 
  head()

# Uso de type_convert() ayuda a corregir problemas detectados en "comentarios".
df_feriados_2$comentarios |> 
  str_trunc(width = 40) |> table(useNA = 'ifany') |> sort()


## Web scraping y el uso de listas ----
# 
# Capturar feriados por año.
# Revision de API: https://apis.digital.gob.cl/fl/
# 
# Para pedir feriados por año: https://apis.digital.gob.cl/fl/feriados/(:año)


url <- 'https://apis.digital.gob.cl/fl/feriados'

# Feriados de 2024
jsonlite::fromJSON('https://apis.digital.gob.cl/fl/feriados/2024') |> head()
jsonlite::fromJSON(paste0(url,'/', 2024)) |> head()

# Ahora capturemos los feriados de 2022 a 2024
anios <- 2022:2024

# Lista con los tres años:
l_anios <- map(anios, \(x) jsonlite::fromJSON(paste0(url,'/', x)))

# Agrego el año a cada elemento de la lista.
l_anios <- set_names(l_anios, anios)

l_anios |> map(head, 2)


# Puedo agregar todos en una sola data.frame mediante list_rbind.
df_anios <- l_anios |> 
  list_rbind(names_to = 'anio') |> 
  as_tibble()

df_anios |> head(2)

glimpse(df_anios)


# stringr Expresiones regulares ----
# 
# Está incluido dentro de los paquetes que carga `library(tidyverse)`.

df_feriados_2$nombre |> table() |> sort()

## str_detect ----
# Detección de feriados asociados a domingo.
df_feriados_2 |> 
  filter(str_detect(nombre, 'Domingo'))
# Luego de 2021 ya no registran el dato en la base.

# Excluir esos datos de la base de datos.
df_feriados_2 |> 
  filter(str_detect(nombre, 'Domingo', negate = TRUE))

df_feriados_2 |> 
  filter(!str_detect(nombre, 'Domingo'))


## str_detect ----
df_feriados_2 |> filter(str_detect(nombre, 'Sábado')) # con tilde

df_feriados_2 |> filter(str_detect(nombre, 'Sabado')) # sin tilde

df_feriados_2 |> filter(str_detect(nombre, 'S.bado')) # comodín

df_feriados_2 |> filter(str_detect(nombre, 'S(a|á)bado')) # a | á

df_feriados_2 |> filter(str_detect(nombre, 'S[aá]bado')) # a | á

## str_subset ----
df_feriados_2$nombre |> str_subset('^Sab') # Parte con "Sab"

## str_replace ----
df_feriados_2$nombre <- df_feriados_2$nombre |> str_replace_all('^Sab', 'Sáb')

# Confirmación de que el cambio fue hecho.
df_feriados_2 |> filter(str_detect(nombre, 'Sabado'))

## str_match_all ----
# 
# Listado de todas las palabras en los nombres de feriados para ver diferencias 
feriados_nombres <- df_feriados_2$nombre |> str_match_all('\\w*')
feriados_nombres |> head()

# Elimino las palabras vacias ""
feriados_nombres_chr <- map(feriados_nombres, # dentro de cada feriado
                            \(x) discard(x, # para cada palabra
                                         \(y) y == ''))

feriados_nombres_chr |> unlist() |> table()

# Reemplazar diferencias detectadas
# 
# old = new
df_feriados_2$nombre |> str_replace_all(
  c('Alcaldes' = 'Alcalde',
    'Parlamentarias' = 'Parlamentaria',
    'Presidenciales' = 'Presidencial')) |> 
  table()

# Listado de reemplazos.
textos_reempazar <- c('Alcaldes' = 'Alcalde',
                      'Parlamentarias' = 'Parlamentaria',
                      'Presidenciales' = 'Presidencial',
                      'Elecciones' = 'Elección',
                      'Segunda Vuelta Gobernadores' = 'Elección Segunda Vuelta Gobernadores',
                      'Plebiscito' = 'Elección',
                      'Ejercito' = 'Ejército',
                      'Prueblos' = 'Pueblos',
                      'todos' = 'Todos')

df_feriados_2$nombre |> 
  str_replace_all(textos_reempazar) |> 
  table()

df_feriados_2$nombre <- df_feriados_2$nombre |> 
  str_replace_all(textos_reempazar)

## Elecciones por año

df_feriados_2 <- df_feriados_2 |> 
  mutate(anio = format(fecha, '%Y'), 
         eleccion = str_detect(nombre, '.*Elecci.*'),
         .after = comentarios)

df_feriados_2 |> 
  filter(eleccion)

df_feriados_2 |> 
  count(anio, wt = eleccion, 
        name = 'n_elecciones')

# Forma equivalente.
df_feriados_2 |> 
  summarise(n_elecciones = sum(eleccion),
            .by = anio)

# Problemas con la información de la API.
# ¿Plebiscito constitucional de salida, por ejemplo?
