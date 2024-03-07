#' ---
#' title: Lab2.2
#' author: Ema Richnakova
#' ---

# logistic regression model to perform a binary classification of football players
# function -> glm(formula, data, family = binomial)

# process of fitting a logistic regression model will require from you to:
# - understand your data,
# - chose the right modeling function (logistic, poisson, etc.),
# - capture and process the information from an object created by the modelling function,
# - understand what logistic model delivers and how decision rules work on modelled probabilities,
# - evaluate error of a model fit, and when needed, refine the model,
# - validate your findings on fresh data.

### task 1
# Load players_22.csv data into R by completing the missing code.

library(tidyverse)
library(magrittr) # Pipe implementation for R
library(data.table) # Implements %like%

#setwd("./cv/cv4") # Set the correct path to players_22.csv
data <- read_csv("players_22.csv", col_names = TRUE, num_threads = 4) # Read data into R
data # Check that data are properly loaded 

### task >2
# A quick glance onto data tells us that there are several player_positions per record.
# To classify a player into one of four classes — offense, defense, midfielder and a goal 
# keeper — use your code from a previous exercise.

data %<>% mutate(
  role = case_when(
    player_positions %like% "ST|RW|LW|CF|CAM" ~ "offense",
    player_positions %like% "LB|LWB|RB|RWB|RB|CDM" ~ "defense",
    player_positions %like% "GK" ~ "goalkeeper",
    .default = "midfielder"
  )
)

### task 3
# Logistic regression models require the dependent variable to be one-hot encoded.
# Your task is to create a new column role_oneHot, containing 1 for offensive players 
# and 0 for other players. Restrict the dataset to only columns you need.

#View(data)
data %<>% 
    mutate(role_oneHot = ifelse(role == "offense", 1, 0)) %<>% # 1 = offensive player, 0 = every other player
    select(short_name,long_name,overall,potential,preferred_foot,dribbling,role,role_oneHot)

### task 4
# Create a multiple logistic regression model to evaluate both hypotheses in one go. 
# Capture the model data into an object and call it (not surprisingly) model.

model <- glm(formula=role_oneHot ~ dribbling + preferred_foot, data=data, family = binomial)
# formula = response variable (0/1 because of binomial) ~ predictor variables
class(model)
summary(model)
str(model)
# View(model) # toto mi nechce zobrazit
plot(model, which=1)

### task 5
# Visualize how the logistic function fits onto our data by completing the following code.
# Without knowing much about the grammar of graphics functions, fill in only variables 
# needed for X and Y axis to be specified.

data %>%
  ggplot(data, mapping=aes(x=dribbling,y=role_oneHot)) + # Define variables for X an Y axix in aes()
  geom_point(position = position_jitter(width = 0.3, height = 0.06), 
             alpha = 0.05, 
             shape = 1, 
             size = 1.5) + 
  stat_smooth(method = glm, method.args = list(family = binomial)) +
  ggtitle("Dribbling Skill Across Player Roles", 
          subtitle = "Offensive players: top, Other players: bottom")  +
  labs(x = "Skill Dribbling", y = "Probability")

### task 6
# Adapt the previous code to discriminate offensive players only from goal keepers. 
# Has the shape of the S-function changed? If yes, why?

data %>%
  filter(role!="goalkeeper")
  ggplot(data, mapping=aes(x=dribbling,y=role_oneHot)) + # Define variables for X an Y axix in aes()
  geom_point(position = position_jitter(width = 0.3, height = 0.06), 
             alpha = 0.05, 
             shape = 1, 
             size = 1.5) + 
  stat_smooth(method = glm, method.args = list(family = binomial)) +
  ggtitle("Dribbling Skill Across Player Roles", 
          subtitle = "Offensive players: top, Other players: bottom")  +
  labs(x = "Skill Dribbling", y = "Probability")

### task 7
# Use knowledge from linear regression exercise to retrieve the coefficients of the logistic function.
# Interpret your findings. You should be intrepreting values like these, or similar.

# TODO - mozno inu funkciu pouzit?
coef <- coef(summary(model))
coef

### task 8
# Find where the probabilities are stored inside model. Save them as model.probabilities and 
# print first 10 values.

# TODO - mierne ine data a aj nedfinovane NA
model.probabilities <- predict(model, data, type = "response")
head(model.probabilities,10)

### task 9
# Convert model.probabilities into one-hot-encoded classes using a cut-off value of 0.5.
# Save your result as glm.prediction.

glm.prediction <- ifelse(model.probabilities >= 0.5, 1, 0)
head(glm.prediction,10)

### task 10
# Perform a cross-tabulation of observed and predicted classes and calculate the associated statistics.
# You can use either the table() function with one-hot-encoded (data$role_oneHot) and
# predicted classes (glm.prediction), or get all work done with the confusionMatrix() 
# function from the caret package.

# TODO - tiez trochu ine data
library(caret)
confusionMatrix(as_factor(glm.prediction), as_factor(data$role_oneHot), positive = "1")
