## -----------------------------------------------------------------------------
#| echo: false
#|
suppressPackageStartupMessages(library(tidyverse))
library(rvest)
library(httr2)

# Clean cookies
invisible({
  files_cookies <- list.files(path = "class_5_files", pattern = 'cookie',
                              full.names = TRUE)
  file.remove(files_cookies)
})


## -----------------------------------------------------------------------------
#| echo: true
text <- "¡Hola! Soy Cristián y tengo 45 años"

(text_url <- utils::URLencode(text))


## -----------------------------------------------------------------------------
#| echo: true
(utils::URLdecode(text_url))


## -----------------------------------------------------------------------------
#| echo: true
resp_bad <- httr2::request('https://blog.desuc.cl/test.html') |> 
  httr2::req_error(is_error = \(x){FALSE}) |> 
  httr2::req_perform()

# httr2::resp_raw(resp_bad)
httr2::resp_status(resp_bad)


## -----------------------------------------------------------------------------
#| echo: true
req_blog  <- httr2::request('https://blog.desuc.cl/') 
resp_blog <- httr2::req_perform(req_blog)

# httr2::resp_raw(resp_blog)
httr2::resp_status(resp_blog)


## -----------------------------------------------------------------------------
#| echo: true
req_blog |> httr2::req_dry_run()


## -----------------------------------------------------------------------------
#| echo: true
resp_blog |> httr2::resp_headers()


## -----------------------------------------------------------------------------
#| echo: true
#|
req_get <- request('http://httpbin.org/get') |> 
  req_url_query(!!!c(var1 = "valor1", 
                     var2 = "valor2")) |>
  req_auth_basic('usuario', 'clavesegura') |> # clave
  req_user_agent('agente desuc httr2')

(resp_get <- req_get |> req_perform())


## -----------------------------------------------------------------------------
resp_get |> resp_body_json() |> str()


## -----------------------------------------------------------------------------
purrr::pluck(resp_get, "request", "headers", "Cookie")


resp <- GET('http://httpbin.org/get',  
            query = list(var1 = "valor1", 
                         var2 = "valor2"),
            authenticate('usuario', 'clavesegura'),
            user_agent(agent = 'agente desuc httr2'),
            ?set_cookies(a = 1),
            accept_json(),
            add_headers(class = "Web Scraping UC", year = 2023)
)

## -----------------------------------------------------------------------------
#| echo: true
resp_github <- request('http://github.com') |> 
  req_cookie_preserve('class_5_files/resp_github_cookies') |> 
  req_perform()

github_cookies <- resp_github |> resp_headers(filter = 'set-cookie')


## -----------------------------------------------------------------------------
#| echo: true
cat(readChar('class_5_files/resp_github_cookies', nchars = 1e4))


## -----------------------------------------------------------------------------
#| echo: true
request('http://httpbin.org/user-agent') |> req_perform() |> 
  resp_body_json()


## -----------------------------------------------------------------------------
#| echo: true
read_html('http://httpbin.org/user-agent') |> 
  html_text()


## -----------------------------------------------------------------------------
#| echo: true
resp_cookies <- request('http://httpbin.org/cookies/set?com_1=tritón&com_2=crema') |>
  req_cookie_preserve('class_5_files/resp_httpbin_cookies') |> 
  req_perform()

resp_cookies


## -----------------------------------------------------------------------------
#| echo: true
cat(readChar('class_5_files/resp_httpbin_cookies', nchars = 1e4))


## -----------------------------------------------------------------------------
resp_cookies |> resp_body_json()


## -----------------------------------------------------------------------------
#| echo: true
resp_cookies2 <- request('http://httpbin.org/cookies') |>
  req_cookie_preserve('class_5_files/resp_httpbin_cookies') |> 
  req_perform() 

resp_cookies2 |> resp_body_json()


## -----------------------------------------------------------------------------
#| echo: true

resp_cookies3 <- request('http://httpbin.org/cookies') |> 
  req_cookie_preserve('class_5_files/resp_httpbin_cookies') |> 
  req_template("/set/:name/:value", 
               name = "com_3", 
               value = "hamburquesa") |>
  req_perform()

cat(readChar('class_5_files/resp_httpbin_cookies', nchars = 1e4))


## -----------------------------------------------------------------------------
resp_cookies3 |> resp_body_json()


## -----------------------------------------------------------------------------
#| echo: true
request('http://httpbin.org/cookies') |> 
  req_perform() |>
  resp_body_json()


## -----------------------------------------------------------------------------
#| echo: true

f_container_row <- function(.req){
  .req |> 
    req_perform() |> 
    resp_body_html() |> 
    html_element('.container .row') |> 
    html_text() |> str_squish()
}

req_advance <- request('https://www.scrapethissite.com/pages/advanced/?gotcha=headers')

try(
  req_advance|> f_container_row()
)


## -----------------------------------------------------------------------------
#| echo: true

try(req_advance |> 
      req_headers(Accept = "text/html") |> 
      f_container_row())


## -----------------------------------------------------------------------------
#| echo: true

try(req_advance |> 
      req_headers(Accept = "text/html") |> 
      req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.5 Safari/605.1.15") |> 
      f_container_row())


## -----------------------------------------------------------------------------
#| echo: true
req_form <- httr::POST('http://quotes.toscrape.com/login',
                   body = list(username = 'test', 
                               password = 'test'))

req_form |> read_html() |> html_element('body form') |> html_text2()


## -----------------------------------------------------------------------------
req_form <- request('http://quotes.toscrape.com/login') |> 
  req_cookie_preserve('class_5_files/resp_form_cookies')

resp_form <- req_form |> 
  req_body_form(username = 'test', 
                password = 'test') |> req_perform()

resp_form |> resp_body_html() |> 
  html_element('body form') |> html_text2()


## -----------------------------------------------------------------------------
cat(readChar('class_5_files/resp_form_cookies', nchars = 1e4))


## -----------------------------------------------------------------------------
#| echo: true
resp_login_html <- req_form |> req_perform() |> 
  resp_body_html() |> html_elements('form input')

resp_login_html


## -----------------------------------------------------------------------------
#| echo: true
(csrf_token_value <- resp_login_html[[1]] |> html_attr('value'))


## -----------------------------------------------------------------------------
#| echo: true
resp_form <- req_form |> 
  req_body_form(username = 'test', 
                password = 'test',
                csrf_token = csrf_token_value) |> req_perform()

resp_form |> resp_body_html() |> 
  html_elements('body .quote') |> html_text2() |> _[1:3]


## -----------------------------------------------------------------------------
s <- rvest::session('http://quotes.toscrape.com/login')
f <- html_form(s)[[1]]

f_llena <- html_form_set(form = f,
                         username = 'test2', 
                         password = 'test2')

f_respuesta <- session_submit(x = s,
                              form = f_llena)

f_respuesta |> read_html() |> 
  html_elements('body .quote') |> 
  html_text2() |> _[1:3]


## -----------------------------------------------------------------------------
#| include: false

# Extraer código R
knitr::purl('class_5.qmd',
            output = 'class_5.R',
            quiet = TRUE)

