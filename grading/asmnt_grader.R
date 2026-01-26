# Set up ----

library(googlesheets4)
library(tidyverse)

gs4_auth("john.spence@trinityschoolnyc.org")

# Function for reading in and cleaning up an assessment gradesheet ----

read_assessment <- function(sheet) {
  
  read_sheet(url, sheet = sheet) |>
    select(name, all_of(sheet)) |>
    filter(!(name %in% c("Total", "Weight", "Multiplier"))) |>
    filter(!is.na(name))
  
}

# Read in and combine all assessment gradesheets ----

url <- "https://docs.google.com/spreadsheets/d/1mfYH40Q3bJPIA8kf-wztFjP_IcrP4j96sVeOVOX75Lk/edit?gid=2018804581#gid=2018804581"

sheets <- sheet_names(url)[1:10]

df_asmnt_join <- sheets |>
  map(read_assessment) |>
  reduce(left_join)

# Replacing big test if they did better on the exam
df_asmnt_exam <- df_asmnt_join |>
  mutate(test1 = if_else(micro_exam > test1, micro_exam, test1),
         test2 = if_else(micro_exam > test2, micro_exam, test2))

# Calculating MP1, MP2 and Sem 1 Averages

df_asmnt_sem1 <- df_asmnt_exam |>
  rowwise() |>
  mutate(quiz_mp1 = mean(c_across(c(ch4_quiz,
                                    ch5_quiz,
                                    ch6_quiz,
                                    ch7_quiz,
                                    ch10_quiz))),
         quiz_mp2 = mean(c_across(c(ch22_quiz, ch14_quiz))),
         test_mp2 = mean(c_across(c(test1, test2))),
         quiz_sem1 = mean(c_across(contains("_quiz"))),
         test_sem1 = test_mp2) |>
  ungroup()
