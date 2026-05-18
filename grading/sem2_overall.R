# Set up ----

library(googlesheets4)
library(tidyverse)

gs4_auth("john.spence@trinityschoolnyc.org")

# Calculate participation, HW, & assessment grades ----

source("~/trinity/econ/grading/sem1_overall.R")

source("~/trinity/econ/grading/sem2_prtcptn.R")

source("~/trinity/econ/grading/sem2_hw.R")

source("~/trinity/econ/grading/sem2_assmnt.R")

rm(list = setdiff(ls(), c("df_sem2_prtcptn",
                          "df_sem2_hw",
                          "df_sem2_assmnt",
                          "df_sem1_final")))

df_sem2_overall <- df_sem2_prtcptn |>
  left_join(df_sem2_hw, by = join_by(name)) |>
  left_join(df_sem2_assmnt, by = join_by(name)) |>
  mutate(
    grade_mp3 = prtcptn_mp3 * 0.1 + hw_mp3 * 0.2 + quiz_mp3 * 0.6 + test_mp3 * 0.1,
    letter_mp3 = case_when(
      grade_mp3 >= 97 ~ "A+",
      grade_mp3 >= 93 ~ "A",
      grade_mp3 >= 90 ~ "A-",
      grade_mp3 >= 87 ~ "B+",
      grade_mp3 >= 83 ~ "B",
      .default = "B-"
    ),
    grade_mp4 = prtcptn_mp4 * 0.1 + hw_mp4 * 0.2 + quiz_mp4 * 0.6 + test_mp4 * 0.1,
    letter_mp4 = case_when(
      grade_mp4 >= 97 ~ "A+",
      grade_mp4 >= 93 ~ "A",
      grade_mp4 >= 90 ~ "A-",
      grade_mp4 >= 87 ~ "B+",
      grade_mp4 >= 83 ~ "B",
      .default = "B-"
    ),
    letter_macro_exam = case_when(
      macro_exam >= 97 ~ "A+",
      macro_exam >= 93 ~ "A",
      macro_exam >= 90 ~ "A-",
      macro_exam >= 87 ~ "B+",
      macro_exam >= 83 ~ "B",
      .default = "B-"
    ),
    grade_sem2 = prtcptn_sem2 * 0.1 + hw_sem2 * 0.2 + quiz_sem2 * 0.5 +
      test_sem2 * 0.1 + macro_exam * 0.1,
    letter_sem2 = case_when(
      grade_sem2 >= 97 ~ "A+",
      grade_sem2 >= 93 ~ "A",
      grade_sem2 >= 90 ~ "A-",
      grade_sem2 >= 87 ~ "B+",
      grade_sem2 >= 83 ~ "B",
      .default = "B-"
    ))

df_full_year <- df_sem1_final |>
  left_join(df_sem2_overall, by = join_by(name)) |>
  select(name, grade_sem1, grade_sem2) |>
  mutate(grade_full_year = (grade_sem1 + grade_sem2) / 2,
         letter_full_year = case_when(
           grade_full_year >= 97 ~ "A+",
           grade_full_year >= 93 ~ "A",
           grade_full_year >= 90 ~ "A-",
           grade_full_year >= 87 ~ "B+",
           grade_full_year >= 83 ~ "B",
           .default = "B-"
         ))
