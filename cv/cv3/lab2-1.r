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
#setwd("./cv") # Set the correct path to players_22.csv
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
    player_positions %like% "ST|RW|LW|CF|CAM" ~ "offense",
    player_positions %like% "LB|LWB|RB|RWB|RB|CDM" ~ "defense",
    player_positions %like% "GK" ~ "goalkeeper",
    .default = "midfielder"
  )
)

### task 3

data %<>%
  select(short_name,role,preferred_foot,player_positions,wage_eur,value_eur,overall,starts_with("skill"))
head(data,5)


### task 4

#-----Helper functions-----
panel.cor <- function(x,y, digits=2, prefix="", cex.cor){
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0,1,0,1))
  r <- abs(cor(x,y,use="complete.obs"))
  txt <- format(c(r,0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt,sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * (1 + r) / 2)
}

panel.hist <- function(x, ...){
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2],0,1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white",...)
}

panel.lm <- function(x,y,col = par("col"), bg = NA, pch = par("pch"),
                     cex = 1, col.smooth = "blue",...){
  points(x,y,pch=pch, col=col(alpha = 0.1), bg=bg, cex=cex)
  abline(stats::lm(x ~ y), col = "steelblue", ...)
} 

#-----Pair plot-----
data %>%
  dplyr::filter(role == "offense") %$%
  pairs( ~ overall + skill_dribbling + skill_curve + skill_fk_accuracy +
           skill_long_passing + skill_ball_control, # Omit skill_moves from plotting       
         upper.panel= NULL, 
         diag.panel=panel.hist,
         lower.panel=panel.smooth)

### Hypothesis Formulation
# Remember, null hypothesis are stated in negative. This is because we have to be able to prove that they are indeed true.

### Model Fitting
# fitting an ordinary linear regression model
# yi=β0+β1x1+...+βpxpi
# To fit an ordinary linear regression model in R, you can use the lm() function.
#
# overall ~ skill_dribbling + preferred_foot to acknowledge the effect of two predictors. This means that for every unit 
# we move on the axis x, there will be two additive effects impacting how much we travel on the y-axis.

### task 4

# this builds regression model
model <- data %>%
  dplyr::filter(role=="offense") %$% # Select offensive players
  lm(formula=overall~skill_dribbling+preferred_foot) # Formalize model parameters
summary(model) # Get more detailed statistics of a model fit

tidy(model) # ina funckia, pytana v task 5

# trochu ine hodnoty mi vyhadzaju so summary ?? mam zle data ?? TODO

### Interpreting Model Coefficients

### task 5

residuals <- model$residuals # Extract residuals from the model object
RSS <- sum(residuals^2) # Calculate residual sum of squares
RMSE <- sqrt(mean(residuals^2)) # Calculate residual root mean squared error
RSS ; RMSE # Print RSS and RMSE