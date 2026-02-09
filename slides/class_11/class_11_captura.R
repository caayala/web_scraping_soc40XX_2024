# title: "Clase 11: GitHub Actions"
# subtitle: "Web scraping y acceso a datos desde la web"
# author: Cristián Ayala
# date: 2024-07-04

library(jsonlite)
library(purrr)

api_url <- "https://www.latercera.com/pf/api/v3/content/fetch/story-feed-query-fetch"
path_out <- "slides/class_11/class_11_files/df_contenido.csv"

api_query <- list(
  feedOffset = 0,
  feedSize = 100,
  fromComponent = "result-list",
  query = "type:story",
  sectionsExclude = paste(
    c(
      "/opinion",
      "/cartas-al-director",
      "/editorial",
      "/que-pasa",
      "/mtonline",
      "/servicios",
      "/el-deportivo",
      "/videos",
      "/branded",
      "/publirreportajes",
      "/emprendimiento",
      "/sustentabilidad",
      "/red-activa",
      "/educacion",
      "/sociales",
      "/lt-board",
      "/mtonline",
      "/club-la-tercera",
      "/paula",
      "/finde",
      "/ai-tiempo"
    ),
    collapse = ", "
  )
)

pluck_or_null <- function(x, path) {
  purrr::pluck(x, !!!path, .default = NULL)
}

first_non_null <- function(...) {
  values <- purrr::keep(list(...), \(v) !is.null(v))
  if (length(values) == 0) {
    return(NULL)
  }
  values[[1]]
}

as_chr_or_na <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  x <- as.character(x[[1]])
  if (!nzchar(trimws(x))) {
    return(NA_character_)
  }
  trimws(x)
}

extract_stories <- function(api_resp) {
  first_non_null(
    api_resp$content_elements,
    pluck_or_null(api_resp, c("content", "elements")),
    pluck_or_null(api_resp, c("result", "content_elements")),
    pluck_or_null(api_resp, c("result", "content", "elements"))
  )
}

stories_as_list <- function(stories) {
  if (is.data.frame(stories)) {
    idx <- seq_len(nrow(stories))
    return(purrr::map(
      idx,
      \(i) {
        purrr::imap(stories, \(col, nm) {
          if (is.data.frame(col)) {
            if (nrow(col) < i) {
              return(NULL)
            }
            return(as.list(col[i, , drop = FALSE]))
          }
          if (length(col) < i) {
            return(NULL)
          }
          col[[i]]
        })
      }
    ))
  }
  if (is.list(stories)) {
    return(stories)
  }
  list()
}

extract_autores <- function(story) {
  autores <- first_non_null(
    pluck_or_null(story, c("credits", "by")),
    pluck_or_null(story, c("credits", "authors"))
  )

  if (is.null(autores) || length(autores) == 0) {
    return(as_chr_or_na(story$byline))
  }

  if (is.data.frame(autores)) {
    nombres <- first_non_null(autores$name, autores$byline)
    nombres <- as.character(nombres)
  } else {
    nombres <- purrr::map_chr(
      autores,
      \(a) as_chr_or_na(first_non_null(a$name, pluck_or_null(a, c("byline"))))
    )
  }

  nombres <- trimws(nombres)
  nombres <- nombres[!is.na(nombres) & nzchar(nombres)]
  if (length(nombres) == 0) {
    return(as_chr_or_na(story$byline))
  }
  paste(nombres, collapse = " y ")
}

extract_story_row <- function(story) {
  titulo <- as_chr_or_na(first_non_null(
    pluck_or_null(story, c("headlines", "basic")),
    story$title
  ))
  tag <- as_chr_or_na(first_non_null(
    pluck_or_null(story, c("label", "basic", "text")),
    pluck_or_null(story, c("taxonomy", "primary_section", "name")),
    pluck_or_null(story, c("headlines", "kicker"))
  ))
  texto <- as_chr_or_na(first_non_null(
    pluck_or_null(story, c("description", "basic")),
    story$subtitle
  ))
  url_imagen <- as_chr_or_na(first_non_null(
    pluck_or_null(story, c("promo_items", "basic", "url")),
    pluck_or_null(story, c("promo_items", "lead_art", "url"))
  ))
  if (is.na(url_imagen)) {
    c_elements <- story$content_elements
    if (is.data.frame(c_elements) && nrow(c_elements) > 0) {
      if ("type" %in% names(c_elements) && "url" %in% names(c_elements)) {
        idx_img <- which(c_elements$type == "image")
        if (length(idx_img) > 0) {
          url_imagen <- as_chr_or_na(c_elements$url[idx_img[1]])
        }
      }
    } else if (is.list(c_elements) && length(c_elements) > 0) {
      urls <- purrr::map_chr(
        c_elements,
        \(el) {
          if (identical(el$type, "image")) {
            return(as_chr_or_na(el$url))
          }
          NA_character_
        }
      )
      urls <- urls[!is.na(urls)]
      if (length(urls) > 0) {
        url_imagen <- urls[[1]]
      }
    }
  }

  data.frame(
    titulo = titulo,
    url_noticia = as_chr_or_na(story$canonical_url),
    tag = tag,
    texto = texto,
    url_imagen = url_imagen,
    periodista = extract_autores(story),
    tiempo = as_chr_or_na(story$display_date),
    fecha_captura = format(Sys.Date(), "%Y-%m-%d"),
    id_noticia = as_chr_or_na(story[["_id"]]),
    tipo_noticia = as_chr_or_na(story$type),
    seccion = as_chr_or_na(pluck_or_null(
      story,
      c("taxonomy", "primary_section", "name")
    )),
    fecha_publicacion = as_chr_or_na(first_non_null(
      story$publish_date,
      story$display_date
    )),
    fecha_actualizacion = as_chr_or_na(story$last_updated_date),
    stringsAsFactors = FALSE
  )
}

bind_fill_rows <- function(df_a, df_b) {
  all_cols <- union(names(df_a), names(df_b))
  for (col in setdiff(all_cols, names(df_a))) {
    df_a[[col]] <- NA_character_
  }
  for (col in setdiff(all_cols, names(df_b))) {
    df_b[[col]] <- NA_character_
  }
  df_a <- df_a[, all_cols, drop = FALSE]
  df_b <- df_b[, all_cols, drop = FALSE]
  rbind(df_a, df_b)
}

fetch_stories_page <- function(feed_offset, query_template) {
  query_page <- query_template
  query_page$feedOffset <- feed_offset
  query_json <- toJSON(query_page, auto_unbox = TRUE)
  req_url <- paste0(api_url, "?query=", URLencode(query_json, reserved = TRUE))
  api_resp <- fromJSON(req_url, simplifyVector = TRUE)
  stories_as_list(extract_stories(api_resp))
}

as_data_frame <- function(df_list) {
  if (length(df_list) == 0) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  do.call(rbind, df_list)
}

max_news_per_day <- 600
page_size <- api_query$feedSize

df_archivo <- NULL
if (file.exists(path_out)) {
  df_archivo <- read.csv2(path_out, stringsAsFactors = FALSE)
}

known_urls <- if (!is.null(df_archivo) && "url_noticia" %in% names(df_archivo)) {
  unique(na.omit(df_archivo$url_noticia))
} else {
  character(0)
}

known_ids <- if (!is.null(df_archivo) && "id_noticia" %in% names(df_archivo)) {
  unique(na.omit(df_archivo$id_noticia))
} else {
  character(0)
}

stop_on_known <- length(known_urls) > 0 || length(known_ids) > 0
offset <- 0
stop_capture <- FALSE
l_new_rows <- list()
n_new <- 0

while (!stop_capture && n_new < max_news_per_day) {
  stories_page <- fetch_stories_page(offset, api_query)
  if (!is.list(stories_page) || length(stories_page) == 0) {
    break
  }

  df_page <- as_data_frame(purrr::map(stories_page, extract_story_row))
  if (nrow(df_page) == 0) {
    break
  }

  known_hit <- rep(FALSE, nrow(df_page))
  if (stop_on_known && "url_noticia" %in% names(df_page)) {
    known_hit <- known_hit | (!is.na(df_page$url_noticia) & df_page$url_noticia %in% known_urls)
  }
  if (stop_on_known && "id_noticia" %in% names(df_page)) {
    known_hit <- known_hit | (!is.na(df_page$id_noticia) & df_page$id_noticia %in% known_ids)
  }

  if (any(known_hit)) {
    first_known <- which(known_hit)[1]
    if (first_known > 1) {
      df_page <- df_page[seq_len(first_known - 1), , drop = FALSE]
    } else {
      df_page <- df_page[0, , drop = FALSE]
    }
    stop_capture <- TRUE
  }

  if (nrow(df_page) > 0) {
    remaining <- max_news_per_day - n_new
    if (nrow(df_page) > remaining) {
      df_page <- df_page[seq_len(remaining), , drop = FALSE]
      stop_capture <- TRUE
    }
    l_new_rows[[length(l_new_rows) + 1]] <- df_page
    n_new <- n_new + nrow(df_page)
  }

  if (length(stories_page) < page_size) {
    break
  }
  offset <- offset + page_size
}

df_nuevo <- as_data_frame(l_new_rows)

if (!is.null(df_archivo)) {
  if (nrow(df_nuevo) > 0) {
    df_bind <- bind_fill_rows(df_archivo, df_nuevo)
  } else {
    df_bind <- df_archivo
  }

  # Deduplicar ignorando fecha_captura y tiempo para mantener continuidad histórica.
  ignore_col <- c("fecha_captura", "tiempo")
  df_bind_subset <- df_bind[, !(names(df_bind) %in% ignore_col), drop = FALSE]
  dup_idx <- duplicated(df_bind_subset)
  df_contenido <- df_bind[!dup_idx, , drop = FALSE]
} else {
  df_contenido <- df_nuevo
}

write.table(
  df_contenido,
  file = path_out,
  quote = TRUE,
  row.names = FALSE,
  col.names = TRUE,
  qmethod = "double",
  fileEncoding = "utf8",
  sep = ";",
  dec = ","
)

# Preparar GitHub Actions ----

# Ayuda desde usethis::use_github_action('check-release')

# usethis::use_github_action('check-release')
#
# > usethis::use_github_action('check-release')
# ✔ Setting active project to '~/Dropbox (DESUC)/Documentos/Clases/UC - Web scraping 2024/web_scraping_soc40XX_2024'
# ✔ Creating '.github/'
# ✔ Adding '^\\.github$' to '.Rbuildignore'
# ✔ Adding '*.html' to '.github/.gitignore'
# ✔ Creating '.github/workflows/'
# ✔ Saving 'r-lib/actions/examples/check-release.yaml@v2' to '.github/workflows/R-CMD-check.yaml'
# • Learn more at <https://github.com/r-lib/actions/blob/v2/examples/README.md>.
# ✔ Adding R-CMD-check badge to 'README.Rmd'
# • Re-knit 'README.Rmd' with `devtools::build_readme()`

## Creación de archivo DESCRIPTION ----

## Permimsos de escritura al workflow o job ----
