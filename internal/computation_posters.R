# libraries --------------------------------------------------------------------
library(tidyverse)
library(janitor)

# data -------------------------------------------------------------------------
submissions <- 
  # read_csv("~/Desktop/CERE2025/submissions.csv") |> 
  googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1aIncHFZ6uTmb0Lz4Wvm9um1CrNjz5W4imuCkK-F5CRs/edit?usp=drive_link") |> 
  clean_names() |> 
  select(nb = docid, abstract) |> 
  mutate(nb = as.character(nb))

posters <- 
  # read_csv("~/Desktop/cere2025_program.csv", col_select = c(1:6)) |> 
  googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1adLAs3GNl0Ww7LTrAeGMjWlAycwF6bk2s_c4bBuQeDg/edit?usp=drive_link",
    range = "A:F",
    col_types = "cccccc"
  ) |> 
  clean_names() |> 
  rename(
    nb = cere2025_program_overview,
    title = x2,
    authors = x5
  ) |> 
  mutate(group = if_else(row_number() < which(nb == "Poster groups’ presentations")[1], NA_character_, nb)) |> 
  drop_na(group) |> 
  mutate(
    group = if_else(str_starts(group, "6"), NA_character_, group),
    day = paste("Poster Session", cumsum(if_else(str_detect(nb,"Poster"), 1, 0)))
  ) |> 
  fill(group, .direction = "down") |> 
  remove_empty("cols") |> 
  drop_na(title) |> 
  left_join(submissions, by = join_by(nb))
