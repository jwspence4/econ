# Setup ----

library(googlesheets4)
library(tidyverse)

# Calculate First Semester Grades ----

source("~/Desktop/econ_grader.R")

rm(read_assessment, grade_quiz, grade_test, grade_cwhw, grade_class)

# Function Definitions ----

read_assessment <- function(sheet, n_assessment, url_assessments) {
  
  read_sheet(url_assessments, sheet) |>
    transmute(name,
              score = if_else(score > 1, 1, score)) |>
    rename_with(~ paste0("assessment", n_assessment), score) |>
    filter(!(name %in% c("Total", "Weight", "Multiplier")))
  
}



grade_quiz <- function(url_assessments) {
  
  df_quiz1 <- read_assessment("Ch. 5 Quiz", 1, url_assessments)
  
  df_quiz2 <- read_assessment("Ch. 5 & 6 Test", 2, url_assessments)
  
  df_quiz3 <- read_assessment("Ch. 5 & 6 Test", 3, url_assessments)
  
  df_quiz4 <- read_assessment("Ch. 7 & 8 Quiz", 4, url_assessments)
  
  df_quiz5 <- read_assessment("Ch. 9 Quiz", 5, url_assessments)
  
  df_quiz6 <- read_assessment("Ch. 10 Quiz", 6, url_assessments)
  
  df_quiz7 <- read_assessment("Ch. 11 Quiz", 7, url_assessments)
  
  df_quiz_all <- df_quiz1 |>
    left_join(df_quiz2, by = "name") |>
    left_join(df_quiz3, by = "name") |>
    left_join(df_quiz4, by = "name") |>
    left_join(df_quiz5, by = "name") |>
    left_join(df_quiz6, by = "name") |>
    left_join(df_quiz7, by = "name") |>
    pivot_longer(cols = contains("assessment"),
                 names_to = "n_assessment",
                 values_to = "score") |>
    mutate(n_assessment = as.numeric(str_replace(n_assessment,
                                                 "assessment",
                                                 "")),
           quarter = if_else(n_assessment <= 4, 3, 4),
           .before = score)
  
  df_quiz <- df_quiz_all |>
    group_by(name) |>
    mutate(sem2_quiz = mean(score)) |>
    group_by(name, quarter) |>
    mutate(q_quiz = mean(score)) |>
    ungroup() |>
    pivot_wider(names_from = quarter,
                names_glue = "q{quarter}_quiz",
                values_from = q_quiz) |>
    group_by(name) |>
    summarize(sem2_quiz = mean(sem2_quiz),
              q3_quiz = mean(q3_quiz, na.rm = TRUE),
              q4_quiz = mean(q4_quiz, na.rm = TRUE)) |>
    ungroup()
}



grade_cwhw <- function(url_cwhw) {
  
  df_cwhw_all <- read_sheet(url_cwhw, "SWYKs/HW", na = "0") |>
    select(name, `Ch. 5`, `Ch. 6`, `Ch. 7`, `Ch. 8`, `Ch. 9`, `Ch. 10`,
           `Ch. 11`) |>
    pivot_longer(!name, names_to = "chapter", values_to = "score")|> 
    mutate(score = if_else(is.na(score), 0, score),
           score = score / 4,
           chapter = str_replace(chapter, "Ch. ", ""),
           chapter = as.numeric(chapter)) |>
    mutate(quarter = if_else(chapter <= 8, 3, 4), .before = score)
  
  df_cwhw <- df_cwhw_all |>
    group_by(name) |>
    mutate(sem2_cwhw = mean(score)) |>
    group_by(name, quarter) |>
    mutate(q_cwhw = mean(score)) |>
    ungroup() |>
    pivot_wider(names_from = quarter,
                names_glue = "q{quarter}_cwhw",
                values_from = q_cwhw) |>
    group_by(name) |>
    summarize(sem2_cwhw = mean(sem2_cwhw),
              q3_cwhw = mean(q3_cwhw, na.rm = TRUE),
              q4_cwhw = mean(q4_cwhw, na.rm = TRUE)) |>
    ungroup()
}



grade_class <- function(url_assessments, url_cwhw) {
  
  df_quiz <- grade_quiz(url_assessments)
  
  df_cwhw <- grade_cwhw(url_cwhw) 
  
  df_prtcptn <- df_quiz |> transmute(name, participation = 1)
  
  df_exam <- read_assessment("Macro Exam", 1, url_assessments) |>
    rename(macro_exam = assessment1)
  
  inc_hw <- c("Will", "Sophia", "Casey", "Elliott", "James", "Johnny", "Ted")
  
  df_hw_fix <- df_prtcptn |>
    left_join(df_cwhw, by = "name") |>
    left_join(df_quiz, by = "name") |>
    left_join(df_exam, by = "name") |>
    separate_wider_delim(name, delim = " ",
                         names = c("first_name", "last_name")) |>
    arrange(last_name) |>
    mutate(finn_tmt = if_else(macro_exam > q4_cwhw, 1, 0),
           finn_tmt = if_else(first_name %in% inc_hw,0, finn_tmt),
           q4_cwhw = if_else(finn_tmt == 1, macro_exam, q4_cwhw),
           sem2_cwhw = if_else(finn_tmt == 1,
                               4 / 7 * q3_cwhw + 3 / 7 * q4_cwhw,
                               sem2_cwhw))
  
  df_overall <- df_hw_fix |>
    mutate(q3_overall = 
             (participation * 0.1 +
                q3_cwhw * 0.3 +
                q3_quiz * 0.5) / 0.9,
           q4_overall = 
             (participation * 0.1 +
                q4_cwhw * 0.3 +
                q4_quiz * 0.5) / 0.9,
           sem2_overall = 
             (participation * 0.1 +
                sem2_cwhw * 0.3 +
                sem2_quiz * 0.5 +
                macro_exam * 0.1)) |>
    transmute(first_name, last_name, participation,
              q3_cwhw, q3_quiz, q3_overall,
              q4_cwhw, q4_quiz, q4_overall,
              sem2_cwhw, sem2_quiz, macro_exam, sem2_overall)
  
}



join_sems <- function(df_sem1, df_sem2) {
  
  df_sem1 |>
    separate_wider_delim(name, delim = " ",
                         names = c("first_name", "last_name")) |>
    arrange(last_name) |>
    rename(micro_exam = exam,
           sem1_cwhw = ytd_cwhw,
           sem1_quiz = ytd_quiz,
           sem1_test = ytd_test,
           sem1_overall = ytd_overall) |>
    select(-participation) |>
    left_join(df_sem2, by = c("first_name", "last_name")) |>
    relocate(participation, .after = last_name) |>
    mutate(ytd_overall = (sem1_overall + sem2_overall) / 2)
  
}



# Calculate grades ---- 

gs4_auth("john.spence@trinityschoolnyc.org")

df_d_sem2 <- grade_class("https://docs.google.com/spreadsheets/d/1A4IRWj02bYgr88BuyfS1wARXUMGkbqXCNQnLBgjjJXg/edit?gid=506989661#gid=506989661",
                         "https://docs.google.com/spreadsheets/d/1UVgEMNoaCdIeQCxhKLPz4A7fBcsq7_nd19tnxeHUaq4/edit?gid=0#gid=0")

df_a_sem2 <- grade_class("https://docs.google.com/spreadsheets/d/1yhzaMkT1Jei3qLJlHg0Bahb8LelKArBy7DkBTA6bQvw/edit?gid=1813745417#gid=1813745417",
                         "https://docs.google.com/spreadsheets/d/1oojIHulRy3ngOqy8cZ-JkO0zqcTF0qprkgZW-apD5Wg/edit?gid=0#gid=0")

df_d_year <- join_sems(df_d_sem1, df_d_sem2)

df_a_year <- join_sems(df_a_sem1, df_a_sem2)

# One offs -----

url_assessments <- "https://docs.google.com/spreadsheets/d/1yhzaMkT1Jei3qLJlHg0Bahb8LelKArBy7DkBTA6bQvw/edit?gid=1813745417#gid=1813745417"
url_cwhw <- "https://docs.google.com/spreadsheets/d/1oojIHulRy3ngOqy8cZ-JkO0zqcTF0qprkgZW-apD5Wg/edit?gid=0#gid=0"
# 
# df_revisions_ytd <- df_quiz_all |>
#   mutate(points_back = (1 - score) / 3,
#          points_back = if_else(n_assessment == 2, points_back * 2, points_back),
#          n_corrections = if_else(name %in% c("Elliott Arbogast", "Andy Yim"),
#                                  1, 2)) |>
#   filter(n_assessment != 3) |>
#   arrange(name, desc(points_back)) |>
#   mutate(n_assessment = case_when(n_assessment == 1 ~ "Ch. 5 Quiz",
#                                   n_assessment == 2 ~ "Ch. 5 & 6 Test",
#                                   n_assessment == 4 ~ "Ch. 7 & 8 Quiz",
#                                   n_assessment == 5 ~ "Ch. 9 Quiz",
#                                   n_assessment == 6 ~ "Ch. 10 Quiz",
#                                   n_assessment == 7 ~ "Ch. 11 Quiz"))
# 
# df_revisions_q4 <- df_revisions_ytd |> filter(quarter == 4)

# df_d_year_fix <- df_d_year |>
#   mutate(finn_tmt = if_else(macro_exam > q4_cwhw, 1, 0),
#          finn_tmt = if_else(first_name %in% c("Will", "Sophia"), 0, finn_tmt),
#          q4_cwhw = if_else(finn_tmt == 1, macro_exam, q4_cwhw))
# 
# df_a_year_fix <- df_a_year |>
#   mutate(finn_tmt = if_else(macro_exam > q4_cwhw, 1, 0),
#          finn_tmt = if_else(first_name %in% inc_hw,
#                             0,
#                             finn_tmt),
#          q4_cwhw = if_else(finn_tmt == 1, macro_exam, q4_cwhw))

