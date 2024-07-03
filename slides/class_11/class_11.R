## -----------------------------------------------------------------------------
#| echo: false
#| warning: false
suppressPackageStartupMessages(library(tidyverse))
library(rvest)
library(httr)


## -----------------------------------------------------------------------------
#| echo = FALSE,
#| include = FALSE

# Extraer código R
knitr::purl('class_11.qmd',
            output = 'class_11.R',
            quiet = TRUE)

