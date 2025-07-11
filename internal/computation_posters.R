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
    "https://docs.google.com/spreadsheets/d/1fWzjjaZC3wEy434IXi7y8IQkUarYu60JlxSBxliEfKE/edit?usp=drive_link",
    range = "A:F",
    col_types = "cccccc"
  ) |> 
  clean_names() |> 
  rename(
    nb = cere2025_program_overview,
    title = x2,
    authors = x5
  ) |> 
  mutate(
    nb = case_match(
      nb, 
      "618447" ~ "Emotions in Daily Life: Interpersonal Perspectives Across Contexts and Cultures - Chair: Davide Pirrone",
      "618540" ~ "In sync or not: What are the correlates of physiological synchronicity? - Chair: Hedwig Eisenbarth",
      .default = nb
    ),
    temp = case_when(
      str_starts(nb, "Poster groups") ~ "poster",
      str_starts(nb, "Thursday") | str_starts(nb, "Friday") ~ "talk",
      .default = NA_character_
    ),
    day = paste("Poster Session", cumsum(case_when(temp == "poster" ~ 1, .default = 0))),
    group = if_else(str_starts(nb, "6"), NA_character_, nb)
  ) |> 
  fill(temp, .direction = "down") |> 
  fill(group, .direction = "down") |> 
  filter(temp == "poster") |> 
  drop_na(authors) |> 
  remove_empty("cols") |> 
  left_join(submissions, by = join_by(nb))
