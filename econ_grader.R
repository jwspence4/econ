# Econ Grading Script
# John Spence
# 1/5/25


# Setup ----

library(googlesheets4)
library(tidyverse)


# Function Definitions ----

read_assessment <- function(sheet, n_assessment, url_assessments) {
  
  read_sheet(url_assessments, sheet) |>
    transmute(name,
              score = if_else(score > 1, 1, score)) |>
    rename_with(~ paste0("assessment", n_assessment), score) |>
    filter(!(name %in% c("Total", "Weight", "Multiplier")))

}



grade_quiz <- function(url_assessments) {
  
  df_quiz1 <- read_assessment("Supply, Demand, Elasticity Quiz",
                              1,
                              url_assessments)
  
  df_quiz2 <- read_assessment("Price Control Quiz",
                              2,
                              url_assessments)
  
  df_quiz3 <- read_assessment(sheet = "Externalities Quiz",
                              3,
                              url_assessments)
  
  df_quiz4 <- read_assessment("Long Run Supply and Monopolies Quiz",
                              4,
                              url_assessments)
  
  df_quiz_all <- df_quiz1 |>
    left_join(df_quiz2, by = "name") |>
    left_join(df_quiz3, by = "name") |> 
    left_join(df_quiz4, by = "name") |>
    pivot_longer(cols = contains("assessment"),
                 names_to = "n_assessment",
                 values_to = "score") |>
    mutate(n_assessment = as.numeric(str_replace(n_assessment,
                                                 "assessment",
                                                 "")),
           quarter = if_else(n_assessment <= 2, 1, 2),
           .before = score)
  
  df_quiz <- df_quiz_all |>
    group_by(name) |>
    mutate(ytd_quiz = mean(score),
           score = if_else(score < ytd_quiz & n_assessment == 1,
                           NA_real_,
                           score),
           ytd_quiz = mean(score, na.rm = TRUE)) |>
    group_by(name, quarter) |>
    mutate(q_quiz = mean(score, na.rm = TRUE)) |>
    ungroup() |>
    pivot_wider(names_from = quarter,
                names_glue = "q{quarter}_quiz",
                values_from = q_quiz) |>
    group_by(name) |>
    summarize(ytd_quiz = mean(ytd_quiz, na.rm = TRUE),
              q1_quiz = mean(q1_quiz, na.rm = TRUE),
              q2_quiz = mean(q2_quiz, na.rm = TRUE)) |>
    ungroup()
  
}



grade_test <- function(url_assessments) {
  
  df_test1 <- read_assessment("Price Control, Taxes Test",
                              1,
                              url_assessments)
  
  df_test2 <- read_assessment(
    "Externalities, Public Goods, Consumer Choice Test",
    2,
    url_assessments
  )
  
  df_test3 <- read_assessment("Consumer Choice, Perfect Competition Test",
                              3,
                              url_assessments)
  
  df_test_all <- df_test1 |>
    left_join(df_test2, by = "name") |>
    left_join(df_test3, by = "name") |>
    pivot_longer(cols = contains("assessment"),
                 names_to = "n_assessment",
                 values_to = "score") |>
    mutate(n_assessment = as.numeric(str_replace(n_assessment,
                                                 "assessment",
                                                 "")),
           quarter = if_else(n_assessment == 1, 1, 2),
           .before = score)
  
  df_test <- df_test_all |>
    group_by(name) |>
    mutate(ytd_test = mean(score)) |>
    group_by(name, quarter) |>
    mutate(q_test = mean(score)) |>
    ungroup() |>
    pivot_wider(names_from = quarter,
                names_glue = "q{quarter}_test",
                values_from = q_test) |>
    group_by(name) |>
    summarize(ytd_test = mean(ytd_test, na.rm = TRUE),
              q1_test = mean(q1_test, na.rm = TRUE),
              q2_test = mean(q2_test, na.rm = TRUE)) |>
    ungroup()
  
}



grade_cwhw <- function(url_cwhw) {
  
  df_cw <- read_sheet(url_cwhw, "Classwork") |>
    pivot_longer(!name, names_to = "date", values_to = "score") |>
    filter(name != "Total Points") |>
    separate_wider_delim(date, delim = "D", names = c("cycle", "day")) |>
    mutate(cycle = str_replace(cycle, "C", ""),
           cycle = as.numeric(cycle),
           day = as.numeric(day),
           score = score / 4,
           drop = case_when(cycle == 1 ~ 1,
                            cycle == 2 & day == 5 ~ 1,
                            .default = 0))
  
  df_hw_unscored <- read_sheet(url_cwhw, "SWYKs/HW", na = "NA") |>
    select(-starts_with("Ch. ")) |>
    pivot_longer(!name, names_to = "date", values_to = "score") |>
    mutate(date = if_else(date %in% c("TestRev1", "TestRev2", "TestRev3"),
                          "C0D0",
                          date)) |>
    separate_wider_delim(date, delim = "D", names = c("cycle", "day")) |>
    mutate(cycle = str_replace(cycle, "C", ""),
           cycle = as.numeric(cycle),
           day = as.numeric(day))
  
  df_hw_totals <- df_hw_unscored |>
    filter(name == "Total Points") |>
    select(-name, possible_pts = score) |>
    distinct()
  
  
  df_hw_scored <- df_hw_unscored |>
    filter(name != "Total Points") |>
    left_join(df_hw_totals, by = c("cycle", "day")) |>
    mutate(score = score / possible_pts) |>
    select(-possible_pts)
  
  df_hw <- df_hw_scored |>
    group_by(name, cycle) |>
    mutate(rank = rank(score, ties.method = "first"),
           drop = case_when(cycle == 0 ~ 0,
                            rank == 1 ~ 1,
                            rank != 1 ~ 0)) |>
    ungroup() |>
    select(-rank)
  
  df_cwhw_drops <- df_cw |> bind_rows(df_hw) |>
    arrange(name, cycle, day) |>
    group_by(name) |>
    mutate(overall_score = mean(score, na.rm = TRUE),
           score_drops = if_else(score < overall_score & drop == 1,
                                 NA_real_,
                                 score),
           overall_score2 = mean(score_drops, na.rm = TRUE),
           score_stable = if_else(overall_score == overall_score2, 1, 0)) |>
    ungroup()
  
  while (sum(df_cwhw_drops$score_stable) != nrow(df_cwhw_drops)) {
    
    df_cwhw_drops <- df_cwhw_drops |>
      group_by(name) |>
      mutate(overall_score = overall_score2,
             score_drops = if_else(score < overall_score & drop == 1,
                                   NA_real_,
                                   score),
             overall_score2 = mean(score_drops, na.rm = TRUE),
             score_stable = if_else(overall_score == overall_score2, 1, 0)) |>
      ungroup()
    
  }
  
  df_cwhw <- df_cwhw_drops |>
    select(-drop, -overall_score2, -score_stable) |>
    mutate(quarter = if_else(cycle < 6, 1, 2), .before = score) |>
    group_by(name, quarter) |>
    summarize(ytd_cwhw = mean(overall_score),
              q_cwhw = mean(score_drops, na.rm = TRUE)) |>
    ungroup() |>
    pivot_wider(names_from = quarter,
                names_glue = "q{quarter}_cwhw",
                values_from = q_cwhw)
  
}



grade_class <- function(url_assessments, url_cwhw) {
  
  df_quiz <- grade_quiz(url_assessments)
  
  df_test <- grade_test(url_assessments)
  
  df_cwhw <- grade_cwhw(url_cwhw) 
  
  df_prtcptn <- df_quiz |> transmute(name, participation = 1)
  
  df_exam <- read_assessment("Micro Exam", 1, url_assessments) |>
    rename(exam = assessment1)
  
  df_overall <- df_prtcptn |>
    left_join(df_cwhw, by = "name") |>
    left_join(df_quiz, by = "name") |>
    left_join(df_test, by = "name") |>
    left_join(df_exam, by = "name") |>
    mutate(ytd_overall =
             participation * 0.1 +
             ytd_cwhw * 0.3 +
             ytd_quiz * 0.2 +
             ytd_test * 0.3 +
             exam * 0.1,
           q2_overall = 
             (participation * 0.1 +
                q2_cwhw * 0.3 +
                q2_quiz * 0.2 +
                q2_test * 0.3) / 0.9,
           q1_overall = 
             (participation * 0.1 +
                q1_cwhw * 0.3 +
                q1_quiz * 0.2 +
                q1_test * 0.3) / 0.9) |>
    relocate(name, participation,
             q1_cwhw, q1_quiz, q1_test, q1_overall,
             q2_cwhw, q2_quiz, q2_test, q2_overall,
             ytd_cwhw, ytd_quiz, ytd_test, exam, ytd_overall)
  
}


# Calculate grades ---- 

gs4_auth("john.spence@trinityschoolnyc.org")

df_d_sem1 <- grade_class("https://docs.google.com/spreadsheets/d/1A4IRWj02bYgr88BuyfS1wARXUMGkbqXCNQnLBgjjJXg/edit?gid=506989661#gid=506989661",
                           "https://docs.google.com/spreadsheets/d/1UVgEMNoaCdIeQCxhKLPz4A7fBcsq7_nd19tnxeHUaq4/edit?gid=0#gid=0")

df_a_sem1 <- grade_class("https://docs.google.com/spreadsheets/d/1yhzaMkT1Jei3qLJlHg0Bahb8LelKArBy7DkBTA6bQvw/edit?gid=1813745417#gid=1813745417",
                           "https://docs.google.com/spreadsheets/d/1oojIHulRy3ngOqy8cZ-JkO0zqcTF0qprkgZW-apD5Wg/edit?gid=0#gid=0")

save(df_d_sem1, file = "~/Desktop/econ_d_sem1.RData")

save(df_a_sem1, file = "~/Desktop/econ_a_sem1.RData")
# url_assessments <- "https://docs.google.com/spreadsheets/d/1yhzaMkT1Jei3qLJlHg0Bahb8LelKArBy7DkBTA6bQvw/edit?gid=1813745417#gid=1813745417"
