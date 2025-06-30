# libraries --------------------------------------------------------------------
library(htmltools)
library(reactable)
library(tidyverse)
library(janitor)

# functions --------------------------------------------------------------------
program_events <- function(program_day, program_rows) {
  program_overview |> 
    filter(day == program_day) |> 
    slice(program_rows) |> 
    mutate(
      level = "#####", # "-"
      jump = "<br>",
      jump2 = "<br><br>"
    ) |> 
    select(level, time, icon, what, jump) |> 
    unite("line", 1:5, sep = " ") |> 
    unlist() |> 
    cat(sep = "\n")
}

program_table <- function(PSSlot_name) {
  reactable_id <- glue::glue("Reactable.toggleAllRowsExpanded('{snakecase::to_any_case(PSSlot_name)}')")
  
  data_program <- program_test |> 
    filter(test == PSSlot_name) |> 
    select(time = cere2025_program_overview, T1 = x2, T2 = x3, T3 = x4, T4 = x5, T5 = x6) |> 
    pivot_longer(-time, names_to = "Track") |> 
    filter(value != "Room:") |> 
    mutate(time = ifelse(is.na(time), "title", time))
  
  data_program_names <- data_program |> 
    filter(time == "title") |> 
    select(Track, TrackName = value)
  
  data_program_table <- data_program |> 
    filter(time != "title") |> 
    left_join(data_program_names, by = join_by(Track))
  
  data_index <- unique(data_program_table[, c("TrackName")])
  
  htmltools::browsable(
    tagList(
      tags$button(
        "Expand/collapse all",
        onclick = reactable_id
      ),
      
      reactable(
        data_index, 
        elementId = snakecase::to_any_case(PSSlot_name),
        bordered = TRUE, 
        striped = TRUE, 
        highlight = TRUE,
        columns = list(
          `TrackName` = colDef(minWidth = 600)
        ),
        details = function(index) {
          data_source <- data_program_table[data_program_table$`TrackName` == data_index$`TrackName`[index], ] |>
            select(time, value)
          htmltools::div(
            style = "padding: 1rem",
            reactable(
              data_source, 
              outlined = TRUE,
              columns = list(time = colDef(maxWidth = 70), value = colDef(maxWidth = 1000))
            )
          )
        }
      )
    )
  )
}
# data -------------------------------------------------------------------------
program_overview <- read_csv("~/Desktop/cere2025_program.csv", col_select = c(1:2)) |> 
  clean_names() |> 
  mutate(
    day = if_else(str_detect(cere2025_program_overview, "^[0-9]"), NA_character_, cere2025_program_overview),
    time = if_else(str_detect(cere2025_program_overview, "-"), cere2025_program_overview, NA_character_),
    what = if_else(row_number() > which(x2 == "Farewells")[1], NA_character_, x2),
    what = if_else(str_starts(what, "Amphi 1: Keynote Speaker"), paste(what, lead(what)), what),
    what = if_else(str_starts(what, "per speaker"), "Parallel Session", what),
    what = str_replace_all(what, c(
      "Amphi 1: Keynote Speaker Steven Heine" = '<a href="https://www.cere2025.com/keynotes/steven_heine.html">Keynote 1: Steven Heine</a>',
      "Amphi 1: Keynote Speaker Agnes Moors" = '<a href="https://www.cere2025.com/keynotes/agnes_moors.html">Keynote 2: Agnes Moors</a>',
      "Amphi 1: Keynote Speaker José-Miguel Fernández-Dols" = '<a href="https://www.cere2025.com/keynotes/jose-miguel_fernandez-dols.html">Keynote 3: José-Miguel Fernández-Dols</a>'
    )),
    icon = case_when(
      str_detect(what, "Registration") ~ "⏰",
      str_detect(what, "Opening|Meeting|Farewells") ~ "🎤",
      str_detect(what, "Keynote") ~ "🎤️",
      str_detect(what, "Coffee") ~ "☕",
      str_detect(what, "Parallel Session") ~ "💬",
      str_detect(what, "Lunch") ~ "🥙",
      str_detect(what, "Poster") ~ "📜"
    )) |> 
  fill(day, .direction = "down") |> 
  select(day, time, what, icon) |> 
  drop_na(what, time)

program_test <- read_csv("~/Desktop/cere2025_program.csv", col_select = c(1:6)) |> 
  clean_names() |> 
  mutate(
    day = if_else(str_detect(cere2025_program_overview, "^[0-9]"), NA_character_, cere2025_program_overview)) |> 
  fill(day, .direction = "down") |> 
  drop_na(x3) |> 
  mutate(test = ifelse(str_detect(x2,"Room"), 1, 0)) |> 
  mutate(test = paste("Parallel Session", cumsum(test)))
