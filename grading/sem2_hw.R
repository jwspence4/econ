# MP 3 HW ----

url_hw <- "https://docs.google.com/spreadsheets/d/1Icb4RL6DfV1aeGphwiCJ9NRHFBuQ3EH3nNI_HoyShdA/edit?gid=0#gid=0"

df_mp3_hw <- read_sheet(url_hw, sheet = "mp3") |>
  mutate(hw_mp3 = 100 * hw_mp3) |>
  filter(!is.na(name))

# MP 4 HW ---
url_hw <- "https://docs.google.com/spreadsheets/d/1Icb4RL6DfV1aeGphwiCJ9NRHFBuQ3EH3nNI_HoyShdA/edit?gid=0#gid=0"

df_mp4_hw <- read_sheet(url_hw, sheet = "mp4") |>
  mutate(hw_mp4 = 100 * hw_mp4) |>
  filter(!is.na(name))

# Sem 2 HW ----
df_sem2_hw <- df_mp3_hw |>
  left_join(df_mp4_hw, by = "name") |>
  rowwise() |>
  mutate(hw_sem2 = mean(c_across(c(-name, -hw_mp3, -hw_mp4))) / 2 * 100) |>
  ungroup() |>
  select(name, hw_mp3, hw_mp4, hw_sem2)
