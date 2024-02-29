#' ---
#' title: Lab1-2
#' author: Ema Richnakova
#' ---

### task1
# Complete the code below (path to players_22.csv)

library(tidyverse)
library(magrittr) # Implements pipes 

getwd() # Find where you are
#setwd("./cv") # Set the correct path to players_22.csv
list.files() # List all files in the working directory

data <- read_csv("players_22.csv", col_names = TRUE, num_threads = 4) # Read data into R
data # Check that data are properly loaded 

### task2
# Restrict data to European’s five major leagues

data[data$league_name %in% c("French Ligue 1","Spain Primera Division","English Premier League","German 1. Bundesliga","Italian Serie A"), ]

### task3
# Complete the code below. In each league, count left- and right-footed players and present these
# data in a tabular format similar to the one below (showing only first 3 lines)

step_1 <- data %>%
  # povodne "German Bundesliga" -> opravene na "German 1. Bundesliga"
  # povodne "Spanish Primera Division" -> opravene na "Spain Primera Division"
  filter(league_name %in% c("French Ligue 1","Spain Primera Division","English Premier League","German 1. Bundesliga","Italian Serie A")) %>%
  group_by(preferred_foot,league_name) %>%
  summarise(Count = n()) %>% # tu bolo povodne summary, to bola asi chyba # alebo tally()
  rename(League = league_name, Pref_Foot = preferred_foot) %>%
  print(n = 3)
#-----Theme defaults-----
theme_set(theme_bw() + 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank()))

#-----Stacked barplot-----
step_1 %$%
  ggplot(., aes(x=reorder(League,Count), y = Count, fill = Pref_Foot)) +
  geom_col() + 
  geom_text(aes(label = format(Count, group = as_factor(Pref_Foot))), 
            colour = "white", position = position_stack(vjust = 0.5)) + 
  scale_fill_manual(values=c('#3153a2', 'lightgrey')) + 
  ggtitle("Letfies in Major European Leagues", 
          subtitle = "Number of Players per League") +
  theme(legend.position="bottom",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "grey")) + 
  coord_flip()

### task4
# Complete the code below. Format data frame step_1 as wide. Calculate total number of 
# players per league and the proportions of left- and right-footed players. Append results to the table.

step_2 <- step_1 %>% 
   pivot_wider(names_from=Pref_Foot,values_from=Count) %>% 
   mutate(Total=Left+Right,Prop_Left=Left/Total,Prop_Right=Right/Total)
step_2

### Lefties are Engaged in Offense ###

### task1
# Complete the code below to extract playing positions. Group by foot preference.
# Restrict to 5 national leagues mentioned above.

step_1 <- data %>%
  filter(league_name %in% c("French Ligue 1","Spain Primera Division","English Premier League","German 1. Bundesliga","Italian Serie A")) %>% # Use filter to select leagues of interest
  select(preferred_foot,player_positions) %>% # Use select to get columns you need
  group_by(preferred_foot) %>% # Use group to aggregate data according to player's foot
  transmute(Position = strsplit(player_positions,", ")) %>% # creates a new data frame containing only the specified computations
  unnest(cols = Position) %>% # Unnest expands a list-column containing data frames into rows and columns
  rename(Pref_Foot = preferred_foot)
step_1 %>% print(n=3)

### task2
# Complete the code below. Use ungroup() to remove previous grouping by foot. 
# Then re-apply group_by() to make a fresh grouping by foot and playing position. 
# Pivot the table wide to get the desired table below.

step_2 <- step_1 %>% 
  ungroup() %>%
  group_by(Pref_Foot,Position) %>% # Group data by foot preference and position  
  summarise(Count=n()) %>% # Generate raw counts for each position
  pivot_wider(names_from=Position,values_from=Count) %>% # Spread
  mutate_all(~coalesce(.x, 0)) # Convert missing values to 0 # alebo cez replace()
step_2

### task3
# Complete the code below. Classify player roles into one of the 3 categories: offense, defense and midfielders.
# Calculate total number of players in each category. Decide on which group CAM and CDM are included.

step_3 <- step_2 %>%
  group_by(Pref_Foot) %>%
  transmute(Offense=CF+LW+RW+ST,
            Defense=LWB+RWB+CB+LB+RB+GK,
            Midfielders=LM+CM+RM+CAM+CDM) # + zgrupuje columns do sum ?
step_3

### task4
# Complete the code below. Absolute numbers does not tells us much because each group has different totals.

step_1 %>%
  group_by(Pref_Foot) %>%
  tally()
# Create a 2 x 3 matrix with population totals. 
popCounts <- matrix(rep(c(1341,3705),3), nrow = 2) 
# Divide the relative counts from step 3 by population totals. 
# Save the resulting matrix as step_4
step_4 <- step_3[,c("Offense","Defense","Midfielders")]/popCounts
step_3
step_4 %>%
  mutate(Foot = step_3$Pref_Foot) %>%
  relocate(Foot) # Rearrange attributes as shown below.
