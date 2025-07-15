# libraries --------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(googlesheets4)
# data -------------------------------------------------------------------------
parallel_sessions <- 
  read_csv(here::here("assets/cere_program.csv"), col_select = c(1:6)) |>
  # googlesheets4::read_sheet(
  #   "https://docs.google.com/spreadsheets/d/1fWzjjaZC3wEy434IXi7y8IQkUarYu60JlxSBxliEfKE/edit?usp=drive_link",
  #   range = "A:F",
  #   col_types = "c"
  # ) |> 
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
  ) |>
  select(time = cere2025_program_overview, T1 = x2, T2 = x3, T3 = x4, T4 = x5, T5 = x6, day, txt) |> 
  pivot_longer(-c(time, day, txt), names_to = "Track") |> 
  filter(!str_starts(value, "Room")) |> 
  mutate(
    time = ifelse(is.na(time), "title", time),
    nb = str_extract(value, "^[0-9]+"),
    value = str_remove(value, "^[0-9]+\\s*")
  )

submissions <- 
  # read_csv("~/Desktop/CERE2025/submissions.csv") |> 
  googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1aIncHFZ6uTmb0Lz4Wvm9um1CrNjz5W4imuCkK-F5CRs/edit?usp=drive_link",
    col_types = "c"
  ) |> 
  clean_names() |> 
  select(nb = docid, abstract) |> 
  mutate(nb = as.character(nb))

# temp <- parallel_sessions
# 
# temp_title <- temp |>
#   filter(time == "title", str_starts(value, "^(Symposium|Data Workshop)")) |>
#   left_join(submissions, by = join_by(nb))
# 
# temp_coms <- temp |>
#   filter(time != "title") |>
#   select(-nb) |>
#   right_join(
#     select(temp_title, txt, Track, Session = value, nb, abstract),
#     by = join_by(txt, Track)) |>
#   bind_rows(temp_title) |>
#   write_csv("symposium_abstracts.csv")
