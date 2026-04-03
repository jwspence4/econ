# Set up ----

library(googlesheets4)
library(tidyverse)

gs4_auth("john.spence@trinityschoolnyc.org")

# MP 3 HW ----

url_hw <- "https://docs.google.com/spreadsheets/d/1Icb4RL6DfV1aeGphwiCJ9NRHFBuQ3EH3nNI_HoyShdA/edit?gid=0#gid=0"

df_mp3_hw <- read_sheet(url_hw, sheet = "mp3") |>
  transmute(name, hw_mp3 = 100 * hw_mp3) |>
  filter(!is.na(name))

# MP 3 Participation ----

url_prtcptn <- "https://docs.google.com/spreadsheets/d/1txSzbsi68jmiiDNZCPDCyrZWl4MQ5wU-fsLs8kmG1dQ/edit?gid=0#gid=0"

df_mp3_prtcptn <- read_sheet(url_prtcptn, sheet = "mp3") |>
  transmute(name, prtcptn_mp3 = 100 * prtcptn_mp3) |>
  filter(!is.na(name))

# MP 3 Assessment ----

# Function for reading in and cleaning up an assessment gradesheet
read_assessment <- function(sheet) {
  
  read_sheet(url_assmnt, sheet = sheet) |>
    select(name, all_of(sheet)) |>
    filter(!(name %in% c("Total", "Weight", "Multiplier"))) |>
    filter(!is.na(name))
  
}

url_assmnt <- "https://docs.google.com/spreadsheets/d/1mfYH40Q3bJPIA8kf-wztFjP_IcrP4j96sVeOVOX75Lk/edit?gid=563868150#gid=563868150"
  
assmnt_sheets <- sheet_names(url_assmnt)[c(11:13, 15)]

df_mp3_assmnt <- assmnt_sheets |>
  map(read_assessment) |>
  reduce(left_join) |>
  mutate(ch25_quiz = ch24_25_quiz) |>
  rename(ch24_quiz = ch24_25_quiz, test_mp3 = test3) |>
  rowwise() |>
  mutate(quiz_mp3 = mean(c_across(c(ch24_quiz,
                                    ch25_quiz,
                                    ch26_quiz,
                                    ch27_quiz)),
                         na.rm = TRUE)) |>
  ungroup()

# MP 3 Overall ----

df_mp3_overall <- df_mp3_hw |>
  left_join(df_mp3_assmnt, by = join_by(name)) |>
  left_join(df_mp3_prtcptn, by = join_by(name)) |>
  select(name, contains("mp3")) |>
  mutate(
    grade_mp3 = prtcptn_mp3 * 0.1 + hw_mp3 * 0.2 + quiz_mp3 * 0.6 + test_mp3 * 0.1,
    letter_mp3 = case_when(
      grade_mp3 >= 97 ~ "A+",
      grade_mp3 >= 93 ~ "A",
      grade_mp3 >= 90 ~ "A-",
      grade_mp3 >= 87 ~ "B+",
      grade_mp3 >= 83 ~ "B",
      .default = "B-"
    )
  )
