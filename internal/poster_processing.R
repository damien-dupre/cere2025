
# libraries --------------------------------------------------------------------
library(tidyverse)
library(here)
library(glue)

# data -------------------------------------------------------------------------
source(here("assets/computations_posters.R"), local = knitr::knit_global())

poster_session_list <- posters |> 
  select(day) |> 
  distinct()

# functions --------------------------------------------------------------------
poster_page_creation <- function(session) {
  file_conn <- 
    glue("program/{snakecase::to_any_case(session)}.qmd") |> 
    file("w")
  
  writeLines(glue(
'---
title: "{{session}}"
execute: 
  echo: false
  
format: 
  html:
    page-layout: full
---

Abstract available when rows are expended:

```{r}
#| include: false

# libraries --------------------------------------------------------------------
library(tidyverse)
library(reactable)
library(here)

# data -------------------------------------------------------------------------
source(here("assets/computations_posters.R"), local = knitr::knit_global())

df <- posters |>
  filter(day == "{{session}}") |>
  select(Nb = nb, Title = title, Authors = authors, Abstract = abstract)
```

```{r}
reactable(
  df,
  pagination = FALSE,
  highlight = TRUE,
  searchable = TRUE,
  columns = list(
    Nb = colDef(maxWidth = 50),
    Title = colDef(minWidth = 400),
    Authors = colDef(minWidth = 200),
    Abstract = colDef(show = FALSE)
  ),
  details = function(index) {
    htmltools::div(
      htmltools::tags$p(paste(df[index, "Abstract"], collapse = "\n"))
    )
  }
  )
```
',
.open = "{{", .close = "}}"), 
  con = file_conn)
  
  close(file_conn)
}

walk(poster_session_list$day, ~poster_page_creation(.x))
