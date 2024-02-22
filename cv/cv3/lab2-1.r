#' ---
#' title: Lab2.1
#' author: Ema Richnakova
#' ---

# data: FIFA 22
# including cols related to:
# short_name, role, preferred_foot, player_positions,
# wage_eur, value_eur, overall
# and all atts names starting with skill.

### Regression Problems

### task 1

library(tidyverse)
library(magrittr)

getwd()
#setwd("./Elements of AI/Data/FIFA 22") # Set the correct path to players_22.csv
data <- read_csv("players_22.csv", col_names = TRUE, num_threads = 4) # Read data into R
#data # Check that data are properly loaded 

### Data Transformation

data["player_positions"] %>% head(n=5)

### task 2

library(data.table) # Implements the %like% operator

# %<>% = bi-directional pipe
# %like% str pattern matching with partial matches
data %<>% mutate(
  role = case_when(
    player_positions %like% c("ST","RW","LW","CF","CAM") ~ "offense",
    player_positions %like% c("LB","LWB","RB","RWB","RB","CDM") ~ "defense",
    player_positions %like% c("GK") ~ "goalkeeper",
    .default = "midfielder"
  )
)
data
