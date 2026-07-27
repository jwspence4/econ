# Set up ----

library(googlesheets4)
library(tidyverse)
library(stringr)

gs4_auth("john.spence@trinityschoolnyc.org")

# HW grades ----

url_hw <- "https://docs.google.com/spreadsheets/d/1OpL9cgZTNJ1JWmQ6dpxOnIZ45dz2gZH0UCwYpzOheLY/edit?gid=0#gid=0"

score_hw <- function(mp) {
  read_sheet(url_hw, sheet = mp) |>
    pivot_longer(-name,
                 names_to = "assignment",
                 values_to = "score") |>
    group_by(name) |>
    summarize("{mp}_hw_avg" := mean(score, na.rm = TRUE)) |>
    ungroup()
}

score_hw("mp1")

# Reading check grades ----

url_rc <- "https://docs.google.com/spreadsheets/d/1ZSvha-Rne0tq02Era-T-HqFHD1Da_bp4PVhVnGAOr4k/edit?gid=0#gid=0"

score_rc <- function(mp) {
  read_sheet(url_rc, sheet = mp) |>
    pivot_longer(-name,
                 names_to = "assignment",
                 values_to = "score") |>
    group_by(name) |>
    summarize("{mp}_rc_avg" := mean(score, na.rm = TRUE)) |>
    ungroup()
}

score_rc("mp1")

#Quiz grades ----

url_quizzes <- "https://docs.google.com/spreadsheets/d/1ZIJC4jhfQZcGSdN7B-VxUrRwITCHkW6gmV_1E3GBtrU/edit?gid=0#gid=0"

quiz_tabs <- sheet_names(url_quizzes)

all_quizzes <- quiz_tabs |>
  map(\(quiz_chptr) {
    read_sheet(url_quizzes, sheet = quiz_chptr) |>
      filter(!is.na(name)) |>
      filter(!(name %in% c("Points", "Weight"))) |>
      transmute(name, score, chptr = parse_number(quiz_chptr))
  }) |>
  bind_rows()

all_quizzes |>
  mutate(mp = case_when(chptr <= 10 ~ "mp1",
                        chptr <= 20 ~ "mp2",
                        chptr <= 30 ~ "mp3",
                        chptr <= 40 ~ "mp4",
                        .unmatched = "error"),
         sem = recode_values(mp, c("mp1", "mp2") ~ "sem1", default = "sem2")) |>
  group_by(name, mp) |>
  mutate(mp_quiz_avg = mean(score)) |>
  group_by(name, sem) |>
  mutate(sem_quiz_avg = mean(score)) |>
  ungroup()



