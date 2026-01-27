# Set up ----

library(tidyverse)



# Calculate HW and Assessment grades ----

source("~/trinity/econ/grading/hw_grader.R")

source("~/trinity/econ/grading/asmnt_grader.R")

rm(list = setdiff(ls(), c("df_hw_sem1", "df_asmnt_sem1")))



# Create aggregate dataset ----

df_sem1_join <- df_hw_sem1 |>
  left_join(df_asmnt_sem1, by = join_by(name)) |>
  mutate(prtcptn = 100)



# Calculate final grades ----

df_sem1_final <- df_sem1_join |>
  select(name, prtcptn, contains(c("mp1", "mp2", "sem1", "sem2", "exam"))) |>
  mutate(grade_mp1 = prtcptn * 0.1 + hw_mp1 * 0.2 + quiz_mp1 * 0.7,
         grade_mp2 = prtcptn * 0.1 + hw_mp2 * 0.2 + quiz_mp2 * 0.6 +
           test_mp2 * 0.1,
         grade_sem1 = prtcptn * 0.1 + hw_sem1 * 0.2 + quiz_sem1 * 0.5 +
           test_sem1 * 0.1 + micro_exam * 0.1)

# df_sem1_join |>
#   select(name, prtcptn, contains(c("mp1", "mp2", "sem1", "sem2"))) |>
#   mutate(grade_sem1 = prtcptn * 0.1 + hw_sem1 * 0.2 + quiz_sem1 * 0.5 + test_sem1 * 0.1) |>
#   filter(name == "Nellie Feldkamp") |>
#   select(grade_sem1)
  