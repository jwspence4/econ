# Set up ----

library(googlesheets4)
library(tidyverse)

gs4_auth("john.spence@trinityschoolnyc.org")

# MP 3 HW ----

hw_url <- "https://docs.google.com/spreadsheets/d/1Icb4RL6DfV1aeGphwiCJ9NRHFBuQ3EH3nNI_HoyShdA/edit?gid=0#gid=0"

df_mp3_hw <- read_sheet(hw_url, sheet = "mp3") |>
  select(name, hw_mp3)

# MP 3 Assessment ----

# Function for reading in and cleaning up an assessment gradesheet
read_assessment <- function(sheet) {
  
  read_sheet(assmnt_url, sheet = sheet) |>
    select(name, all_of(sheet)) |>
    filter(!(name %in% c("Total", "Weight", "Multiplier"))) |>
    filter(!is.na(name))
  
}

assmnt_url <- "https://docs.google.com/spreadsheets/d/1mfYH40Q3bJPIA8kf-wztFjP_IcrP4j96sVeOVOX75Lk/edit?gid=563868150#gid=563868150"
  
assmnt_sheets <- sheet_names(assmnt_url)[c(11:13, 15)]

df_mp3_asmnt <- assmnt_sheets |>
  map(read_assessment) |>
  reduce(left_join) |>
  rename(test_mp3 = test3) |>
  rowwise() |>
  mutate(quiz_mp3 = mean(c_across(c(ch24_25_quiz,
                                    ch26_quiz,
                                    ch27_quiz)))) |>
  ungroup()
