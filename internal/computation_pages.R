# poster pages -----------------------------------------------------------------
sessions <- paste("Poster Session", 1:3)
template <- readr::read_file("internal/poster_session_template.qmd")

page_creation <- function(session) {
  file_conn <- 
    glue::glue("program/{snakecase::to_any_case(session)}.qmd") |> 
    file("w")
  
  writeLines(glue::glue(
    template,
    .open = "{{", .close = "}}"), # glue double fenced because of code chunk capsule
    con = file_conn)
  
  close(file_conn)
}

purrr::walk(sessions, ~page_creation(.x))

# session pages ----------------------------------------------------------------
source(here::here("internal/computation_talks.R"), local = knitr::knit_global())

sessions <- parallel_sessions |> 
  distinct(txt, Track) |> 
  rename(session = txt, track = Track)

template <- readr::read_file("internal/parallel_session_template.qmd")

page_creation <- function(session, track) {
  file_conn <- 
    glue::glue("program/{paste(snakecase::to_any_case(session), track, sep = '_')}.qmd") |> 
    file("w")
  
  writeLines(glue::glue(
    template,
    .open = "{{", .close = "}}"), # glue double fenced because of code chunk capsule
    con = file_conn)
  
  close(file_conn)
}

purrr::pwalk(sessions, page_creation)

