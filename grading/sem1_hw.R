library(googlesheets4)
library(tidyverse)

# Read in marking period 2 Gradescope data and drop irrelevant columns
hw_path <- "~/trinity/econ/grading/Principles_of_Economics_Fall_2025_grades.csv"

df_hw <- read_csv(hw_path) |>
  select(`First Name`,
         `Last Name`,
         contains(c("10", "11", "22", "14", "15")),
         -contains(c("Time", "Lateness", "Corrections")))

# Clean up column names for pivots
df_hw_names <- df_hw |>
  rename_with(~str_replace_all(., "HW 10.2.-10.3.", "Ch. 10 HW #1") |>
                str_replace_all("HW 10.4.", "Ch. 10 HW #2")) |>
  rename_with(~str_to_lower(.) |>
                str_replace_all("[(#). ]", "") |>
                str_replace_all("-maxpoints", "_max")) |>
  rename_with(
    .fn = ~paste0(., "_score"),
    .cols = -c(firstname, lastname) & !ends_with("_max")
  )

# Calculate scores as percents
df_hw_long <- df_hw_names |>
  pivot_longer(
    cols = -c(firstname, lastname),
    names_to = c("assignment", ".value"), 
    names_pattern = "(.*)_(.*)"
  ) |>
  mutate(
    # score = if_else(is.na(score), 0, score),
    percent = score / max * 100
  )

# Combine assignments that differed across periods
df_hw_combo <- df_hw_long |>
  select(-score, -max) |>
  mutate(
    assignment = if_else(
      assignment %in% c("ch15cw1cperiod", "ch15hw3fperiod"),
      "ch15other",
      assignment
    ),
    assignment = str_remove(assignment, "[cf]period")
  ) |>
  filter(!is.na(percent)) |>
  pivot_wider(
    names_from = assignment,
    values_from = percent
  )

# Add in late assignments
df_hw_lates <- df_hw_combo |>
  mutate(
    ch10hw2 = case_when(
      firstname == "A.J." ~ 75,
      lastname == "Nangle" ~ 75,
      .default = ch10hw2
    ),
    ch11cw = case_when(
      lastname == "Abush" ~ 75,
      lastname == "Maggio" ~ 75,
      firstname == "A.J." ~ 75,
      lastname == "Nangle" ~ 75,
      firstname == "Leo" ~ 75,
      .default = ch11cw
    ),
    ch11hw = case_when(
      lastname == "Maggio" ~ 75,
      lastname == "Nangle" ~ 75,
      firstname == "Neev" ~ 75,
      .default = ch11hw
    ),
    ch14cw1 = case_when(
      lastname == "Bader" ~ 20/22 * 100,
      lastname == "Gianopulos" ~ 100,
      lastname == "Aronstein" ~ 75,
      lastname == "Nangle" ~ 20 / 22 * 75,
      .default = ch14cw1
    ),
    ch14hw1 = case_when(
      firstname == "Neev" ~ 75,
      lastname == "Nangle" ~ 75,
      .default = ch14hw1
    ),
    ch15other = case_when(
      lastname == "Gianopulos" ~ 75,
      firstname == "Diana" ~ 75,
      lastname == "Iofin" ~ 100,
      firstname == "Ellan" ~ 75,
      firstname == "Sonia" ~ 75,
      lastname == "Barnett" ~ 75,
      lastname == "Bender" ~ 75,
      lastname == "Nangle" ~ 75,
      firstname == "Neev" ~ 75,
      firstname == "Nico" ~ 75,
      lastname == "Sorgen" ~ 11 / 12 * 75,
      firstname == "Jackson" ~ 75,
      lastname == "Abush" ~ 75,
      firstname == "Triton" ~ 75,
      .default = ch15other
    ),
    ch15hw2 = case_when(
      firstname == "Neev" ~ 100,
      firstname == "Diana" ~ 75,
      lastname == "Barnett" ~ 75,
      firstname == "Leo" ~ 75,
      firstname == "Alegra" ~ 10 / 12 * 75,
      .default = ch15hw2
    ),
    ch22hw4 = case_when(
      lastname == "Smith" ~ 100,
      firstname == "Neev" ~ 75,
      firstname == "A.J." ~ 75,
      lastname == "Nangle" ~ 75,
      .default = ch22hw4
    ),
    ch22cw2 = case_when(
      lastname == "Smith" ~ 100,
      firstname == "Neev" ~ 75,
      firstname == "A.J." ~ 75,
      lastname == "Nangle" ~ 75,
      .default = ch22cw2
    )
  )

# Calculate marking period 2 average
df_hw_mp2 <- df_hw_lates |>
  mutate(across(.cols = -c(firstname, lastname),
                .fns = ~replace_na(., 0))) |>
  rowwise() |>
  mutate(hw_mp2 = mean(c_across(-c(firstname, lastname)))) |>
  ungroup()

#Import marking period 1 HW
gs4_auth("john.spence@trinityschoolnyc.org")

url_hw_mp1 <- "https://docs.google.com/spreadsheets/d/1Icb4RL6DfV1aeGphwiCJ9NRHFBuQ3EH3nNI_HoyShdA/edit?gid=0#gid=0"

df_hw_mp1 <- read_sheet(url_hw_mp1, sheet = "q1") |>
  mutate(across(.cols = contains("ch"),
                .fns = ~ . * 100))

#Calculate sem 1 average
df_hw_sem1 <- df_hw_mp1 |>
  left_join(df_hw_mp2, c("firstname", "lastname")) |> #combine mp 1 & 2
  rowwise() |>
  mutate(hw_sem1 = mean(c_across(-c(firstname, lastname, hw_mp1, hw_mp2)))) |>
  ungroup() |>
  unite("name", firstname, lastname, sep = " ")

# df_hw_sem1 |> filter(if_any(everything(), ~ . == 0)) |> View()
