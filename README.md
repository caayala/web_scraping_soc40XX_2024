
<!-- README.md is generated from README.Rmd. Please edit that file -->

### Web Scraping y acceso a datos desde la web

<!-- badges: start -->
<!-- badges: end -->

Este repositorio contiene el material del curso Web Scraping y acceso a
datos desde la web, dictado el primer semestre de 2024 por el
Departamento de Sociología de la Universidad Católica de Chile a
estudiantes de educación continua como parte del
[`Diplomado en Web Scraping y visualización de datos sociales en R`](https://educacioncontinua.uc.cl/programas/webscraping-y-acceso-a-datos-desde-la-web/).

El programa de este curso se encuentra [`acá`](files/01-programa.pdf).

Al final de este curso los alumnos debiesen tener la capacidad de
acceder a nuevas fuentes de datos para su análisis. Esta habilidad es de
gran utilidad práctica porque más y más información es generada,
almacenada y —de alguna manera— disponible en Internet.

El sitio web del curso es
<https://caayala.github.io/web_scraping_soc40XX_2024/>.

------------------------------------------------------------------------

#### Calendario

Tendremos clases teóricas (T) y prácticas (P).

| Clase | fecha      | tipo | contenido                                          | Paquetes                                                                                     | entrega                       | material                                                                                                    |
|------:|:-----------|:-----|:---------------------------------------------------|:---------------------------------------------------------------------------------------------|:------------------------------|:------------------------------------------------------------------------------------------------------------|
|     1 | 2024-05-28 | T    | Introducción a Web Scraping                        | [`rvest`](https://rvest.tidyverse.org)                                                       |                               | [Slides](slides/class_1/class_1#1) [.qmd](slides/class_1/class_1.qmd)                                       |
|     2 | 2024-05-30 | P    | Repaso R: manejo de listas y expresiones regulares | [`purrr`](https://purrr.tidyverse.org) [`stringr`](https://stringr.tidyverse.org)            |                               | [Slides](slides/class_2/class_2#1) [.qmd](slides/class_2/class_2.qmd) [.R](slides/class_2/class_2_taller.R) |
|     3 | 2024-06-04 | T    | Web Scraping                                       | [`rvest`](https://rvest.tidyverse.org)                                                       |                               | [Slides](slides/class_3/class_3#1) [.qmd](slides/class_3/class_3.qmd)                                       |
|     4 | 2024-06-06 | P    | Web Scraping                                       | [`rvest`](https://rvest.tidyverse.org)                                                       | [C1 (20%)](homework/c_1.html) | [Slides](slides/class_4/class_4#1) [.qmd](slides/class_4/class_4.qmd) [.R](slides/class_4/class_4_taller.R) |
|     5 | 2024-06-11 | T    | Web Scraping avanzado                              | [`rvest`](https://rvest.tidyverse.org), [`httr2`](https://httr2.r-lib.org)                   |                               | [Slides](slides/class_5/class_5#1) [.qmd](slides/class_5/class_5.qmd)                                       |
|     6 | 2024-06-13 | P    | Web Scraping avanzado                              | [`rvest`](https://rvest.tidyverse.org), [`httr2`](https://httr2.r-lib.org)                   | [C2 (20%)](homework/c_2.html) | [Slides](slides/class_6/class_6#1) [.qmd](slides/class_6/class_6.qmd) [.R](slides/class_6/class_6_taller.R) |
|     7 | 2024-06-18 | T    | Integración de R con Google Sheets                 | [`googlesheets4`](https://googlesheets4.tidyverse.org)                                       |                               | [Slides](slides/class_7/class_7#1) [.qmd](slides/class_7/class_7.qmd)                                       |
|       | 2024-06-20 |      | *Feriado*                                          |                                                                                              |                               |                                                                                                             |
|     8 | 2024-06-25 | P    | Integración de R con Google Sheets                 | [`googlesheets4`](https://googlesheets4.tidyverse.org)                                       | Propuesta TF                  |                                                                                                             |
|     9 | 2024-06-27 | T    | Uso de APIs: Spotify                               | [`spotifyr`](https://www.rcharlie.com/spotifyr/)                                             |                               |                                                                                                             |
|    10 | 2024-07-02 | T    | Uso headless browser: chromote                     | [`chormote`](https://rstudio.github.io/chromote/)                                            |                               |                                                                                                             |
|    11 | 2024-07-04 | T    | Captura automatica y presentación trabajo final    | [`usethis`](https://usethis.r-lib.org), [GitHub Actions](https://docs.github.com/en/actions) |                               |                                                                                                             |

#### Evaluaciones

- Control 1 (20%): Captura de página web 1.
  [Instrucciones](./homework/c_1).
  <!-- [Respuesta](./homework/c_1_answers.pdf). -->

- Control 2 (20%): Captura de página web 2: Captura de datos desde IMBD.
  [Instrucciones](./homework/c_2).
  <!-- [Respuesta](./homework/c_2_answers.pdf). -->

- Control 3 (20%): Uso de API <!-- Análisis de canciones en Spotify. -->
  <!-- [Instrucciones](./homework/c_3). -->
  <!-- [Respuesta](./homework/c_3_answers.pdf) -->

- Trabajo Final (40%): Trabajo de interés de los alumnos.

------------------------------------------------------------------------

#### Requisitos

- Descargar e instalar [R 4.4](https://cran.r-project.org)
- [RStudio](https://posit.co/downloads/)

#### Recursos de aprendizaje

- [CSS Selectors](https://www.w3schools.com/css/css_selectors.asp): A
  CSS selector selects the HTML element(s) you want to style.
- [Try selectors](https://www.w3schools.com/CSSref/trysel.php): Click a
  selector to see which element(s) that gets selected in the result.
- [CSS Diner](https://flukeout.github.io): It’s a fun game to learn and
  practice CSS selectors. Repo en
  [Github](https://github.com/flukeout/css-diner)
- [Public API
  Lists](https://github.com/public-api-lists/public-api-lists): A
  collective list of free APIs for use in software and web development.
- [Visual Studio Code](https://code.visualstudio.com)
  - [REST
    Client](https://marketplace.visualstudio.com/items?itemName=humao.rest-client):
    REST Client for Visual Studio Code

#### Lecturas y referencias

- **R for Data Science (2e)** (Hadley Wickham & Garrett Grolemund)
  [web](https://r4ds.hadley.nz/index.html).
- **`rvest` Web scraping 101**
  [web](https://rvest.tidyverse.org/articles/rvest.html)
- **Web Scraping Reference: Cheat Sheet for Web Scraping using R**
  [github](https://github.com/yusuzech/r-web-scraping-cheat-sheet)
- **`dplyr` cheatsheet**
  [pdf](https://raw.githubusercontent.com/rstudio/cheatsheets/main/dplyr.pdf)
- **`purrr` cheatsheet**
  [pdf](https://raw.githubusercontent.com/rstudio/cheatsheets/main/purrr.pdf)
- **`stringr` cheatsheet**
  [pdf](https://raw.githubusercontent.com/rstudio/cheatsheets/main/strings.pdf)
