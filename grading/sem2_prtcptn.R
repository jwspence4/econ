# Sem 2 Participation ----

url_prtcptn <- "https://docs.google.com/spreadsheets/d/1txSzbsi68jmiiDNZCPDCyrZWl4MQ5wU-fsLs8kmG1dQ/edit?gid=0#gid=0"

df_sem2_prtcptn <- read_sheet(url_prtcptn, sheet = "sem2") |>
  mutate(prtcptn_mp3 = 100 * prtcptn_mp3,
         prtcptn_mp4 = 100 * prtcptn_mp4,
         prtcptn_sem2 = 7 / 12 * prtcptn_mp3 + 5 / 12 * prtcptn_mp4) |>
  filter(!is.na(name))