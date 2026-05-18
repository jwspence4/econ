# MP 3 Assessment ----

# Function for reading in and cleaning up an assessment gradesheet
read_assessment <- function(sheet) {
  
  read_sheet(url_assmnt, sheet = sheet) |>
    select(name, all_of(sheet)) |>
    filter(!(name %in% c("Total", "Weight", "Multiplier"))) |>
    filter(!is.na(name))
  
}

url_assmnt <- "https://docs.google.com/spreadsheets/d/1mfYH40Q3bJPIA8kf-wztFjP_IcrP4j96sVeOVOX75Lk/edit?gid=563868150#gid=563868150"

sheet_names(url_assmnt)

assmnt_sheets <- c("ch24_25_quiz",
                   "ch26_quiz",
                   "ch27_quiz",
                   "test3",
                   "ch31_quiz",
                   "ch34_quiz",
                   "ch35_quiz",
                   "test4",
                   "macro_exam")

df_sem2_assmnt <- assmnt_sheets |>
  map(read_assessment) |>
  reduce(left_join) |>
  mutate(ch25_quiz = ch24_25_quiz) |>
  rename(ch24_quiz = ch24_25_quiz, test_mp3 = test3, test_mp4 = test4) |>
  rowwise() |>
  mutate(quiz_mp3 = mean(c_across(c(ch24_quiz,
                                    ch25_quiz,
                                    ch26_quiz,
                                    ch27_quiz)),
                         na.rm = TRUE),
         quiz_mp4 = mean(c_across(c(ch31_quiz,
                                    ch34_quiz,
                                    ch35_quiz)),
                         na.rm = TRUE),
         quiz_sem2 = mean(c_across(c(ch24_quiz,
                                     ch25_quiz,
                                     ch26_quiz,
                                     ch27_quiz,
                                     ch31_quiz,
                                     ch34_quiz,
                                     ch35_quiz)),
                          na.rm = TRUE),
         test_sem2 = mean(c_across(c(test_mp3, test_mp4)),
                          na.rm = TRUE)) |>
  ungroup() |>
  mutate(ch35_quiz = if_else(is.na(ch35_quiz), quiz_sem2, ch35_quiz),
         test_mp4 = if_else(is.na(test_mp4), quiz_sem2, test_mp4),
         macro_exam = if_else(is.na(macro_exam), quiz_sem2, macro_exam)) |>
  mutate(test_mp3 = if_else(test_mp3 < macro_exam, macro_exam, test_mp3),
         test_mp4 = if_else(test_mp4 < macro_exam, macro_exam, test_mp4)) |>
  select(-contains("ch"))

