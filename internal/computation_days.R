# libraries --------------------------------------------------------------------
library(htmltools)
library(reactable)
library(tidyverse)
library(janitor)

# data -------------------------------------------------------------------------
program_overview <- 
  # read_csv("~/Desktop/cere2025_program.csv", col_select = c(1:2)) |> 
  googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1fWzjjaZC3wEy434IXi7y8IQkUarYu60JlxSBxliEfKE/edit?usp=drive_link",
    range = "A:B",
    col_types = "cc"
  ) |>
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

parallel_sessions <- 
  # read_csv("~/Desktop/cere2025_program.csv", col_select = c(1:6)) |> 
  googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1fWzjjaZC3wEy434IXi7y8IQkUarYu60JlxSBxliEfKE/edit?usp=drive_link",
    range = "A:F",
    col_types = "cccccc"
  ) |>
  clean_names() |> 
  mutate(
    day = if_else(str_detect(cere2025_program_overview, "^[0-9]"), NA_character_, cere2025_program_overview)) |> 
  fill(day, .direction = "down") |> 
  drop_na(x3) |> 
  mutate(txt = ifelse(str_detect(x2,"Room"), 1, 0)) |> 
  mutate(txt = paste("Parallel Session", cumsum(txt))) |> 
  add_row(
    cere2025_program_overview = "11.15", 
    x5 = "609342 Data Workshop - Damien Dupré - Identifying Correct or Incorrect Emotion Recognition from Facial Expression Time",
    day = "Wednesday 16th July",
    txt = "Parallel Session 2"
  )

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

program_table <- function(parallel_session) {
  reactable_id <- glue::glue("Reactable.toggleAllRowsExpanded('{snakecase::to_any_case(parallel_session)}')")
  
  data_program <- parallel_sessions |> 
    filter(txt == parallel_session) |> 
    select(time = cere2025_program_overview, T1 = x2, T2 = x3, T3 = x4, T4 = x5, T5 = x6) |> 
    pivot_longer(-time, names_to = "Track")
  
  data_room <- data_program |> 
    filter(str_starts(value, "Room")) |> 
    mutate(Room = value |> 
             str_remove("Room: ") |> 
             str_to_title()
    ) |> 
    select(Track, Room)
  
  data_program <- data_program |> 
    filter(!str_starts(value, "Room")) |> 
    mutate(
      time = ifelse(is.na(time), "title", time),
      value = if_else(str_detect(value, "^[0-9]"), str_sub(value, 8), value)
    ) |> 
    left_join(data_room, by = join_by(Track))
  
  data_program_names <- data_program |> 
    filter(time == "title") |> 
    separate(value, c("Parallel Sessions", "Chair"), sep = " Chair: ") |> 
    mutate(Link = glue::glue("../{snakecase::to_any_case(parallel_session)}_{Track}.html")) |> 
    select(-time, -Track)
  
  data_program_table <- data_program |> 
    filter(time != "title") |> 
    left_join(data_program_names, by = join_by(Room)) |> 
    rename(Time = time, Communications = value)
  
  htmltools::browsable(
    tagList(
      tags$button(
        "Expand/collapse all",
        onclick = reactable_id
      ),
      
      reactable(
        data_program_names, 
        elementId = snakecase::to_any_case(parallel_session),
        bordered = TRUE, 
        striped = TRUE, 
        highlight = TRUE,
        columns = list(
          `Parallel Sessions` = colDef(minWidth = 500),
          Chair = colDef(minWidth = 200),
          Link = colDef(maxWidth = 125, cell = function(value, index) {
            htmltools::tags$a(class = "btn btn-default", href = value, target = "_blank", "Abstracts")
          })
        ),
        details = function(index) {
          data_source <- data_program_table[data_program_table$`Parallel Sessions` == data_program_names$`Parallel Sessions`[index], ] |>
            select(Time, Communications)
          htmltools::div(
            style = "padding: 1rem",
            reactable(
              data_source, 
              outlined = TRUE,
              columns = list(Time = colDef(maxWidth = 70), Communications = colDef(maxWidth = 1000))
            )
          )
        }
      )
    )
  )
}
