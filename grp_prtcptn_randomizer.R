# Table Group & Participation Randomizer ----
# John Spence
# 8/25/2024
library(tidyverse)

# Define rosters ----
econ_c <- c(
  "Ellan",
  "Max",
  "Zach",
  "Edmund",
  "Henry",
  "Oliver",
  "Nellie",
  "Josh",
  "Ishaan",
  "Sonia",
  "John",
  "Neev",
  "Leo",
  "Soham",
  "Jackson",
  "Michael",
  "Raana",
  "Diana",
  "Sanaii",
  "Brooke"
)

econ_f <- c(
  "Ale",
  "A.J.",
  "James",
  "Sophia B.",
  "Jordan",
  "Sophia G.",
  "Daniel",
  "Sam",
  "Maxine",
  "Win",
  "Triton",
  "Collin",
  "Julia",
  "Alegra",
  "Madhav",
  "Simon",
  "Elena",
  "Vedant",
  "Blake",
  "Emma"
)

im2_d <- c(
  "Kamilla",
  "Violet",
  "Sophia",
  "Paula",
  "Avery",
  "Edie",
  "Veronika",
  "Oscar",
  "Sachin",
  "Tyler",
  "Anya",
  "Mina",
  "Sebastian",
  "Mia"
)

im1_a <- c(
  "Adi",
  "Sam",
  "Connor",
  "Sawyer",
  "Catalina",
  "Ishaani",
  "Alexia",
  "Ryan",
  "Declan",
  "Sophie",
  "Henry",
  "Tayhan",
  "Marco",
  "Darya",
  "Harrison",
  "Jade"
)

# Table Randomizer ----

tbl_grps <- function(class) {
  
  if (length(class) < 15) { n_grp = 4 } else { n_grp = 5 }
  
  tbl_n <- rep_along(class, 1:n_grp) |> sample(size = length(class))
  
  df_nmbrd <- tibble(name = class, tbl_n = tbl_n)
  
  df_nmbrd |>
    arrange(tbl_n) |>
    group_by(tbl_n) |>
    mutate(n = row_number(tbl_n)) |>
    pivot_wider(names_from = tbl_n,
                names_prefix = "Table ",
                values_from = name,
                values_fill = "") |>
    select(-n)

}

# Select Table Groups ----
tbl_grps(econ_c)

tbl_grps(econ_f)

tbl_grps(im2_d)

tbl_grps(im1_a)

# Participation randomizer ----
im2_d |> sample(4)
im1_a |> sample(4)
