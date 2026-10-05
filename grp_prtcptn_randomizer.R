# Table Group & Participation Randomizer ----
# John Spence
# 8/25/2024
library(tidyverse)

# Define rosters ----
econ_c <- c(
  "Sydney",
  "Theo",
  "Elliot",
  "Alexis",
  "Evan",
  "Caitlin",
  "Adrian",
  "Charlie",
  "Sam",
  "Siya",
  "Ryan",
  "Aidan",
  "Anders",
  "Forest",
  "Caden",
  "Quinn",
  "Ilan",
  "Isabella"
)

econ_e <- c(
  "Armaan",
  "Adele",
  "Samo",
  "Maddy",
  "Kian",
  "Fahad",
  "Caroline",
  "Max",
  "Allen",
  "Jared",
  "Jacob",
  "Sami",
  "Cooper",
  "Luke"
)

im1_g <- c(
  "Owen",
  "Hoda",
  "Aidan",
  "Arianna",
  "Alexander",
  "Naveed",
  "Maia",
  "Daniel",
  "Hassanata",
  "Jackson",
  "Laila",
  "Jonathan",
  "Sofia",
  "Luca"
)

# Table Randomizer ----

tbl_grps <- function(class) {
  
  n_grp <- length(class) %/% 3
  
  # n_grp <- ceiling(length(class) / 3)
  
  tbl_n <- rep_along(class, 1:n_grp) |> sample(size = length(class))
  
  df_nmbrd <- tibble(name = class, tbl_n = tbl_n)
  
  df_nmbrd |>
    arrange(tbl_n) |>
    group_by(tbl_n) |>
    mutate(n = row_number(tbl_n)) |>
    pivot_wider(names_from = tbl_n,
                names_prefix = "Group ",
                values_from = name,
                values_fill = "") |>
    select(-n)

}

# Select Table Groups ----
tbl_grps(econ_c)

tbl_grps(econ_e)

tbl_grps(im1_g)

