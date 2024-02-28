###############
### TUT 3.1 ###
###############

### Functions ###

fun <- function(x) {
  sin(1 / x ^ 2)
}

### task 1

f <- function(abcdef, bcde1, bcde2) {
  list(a = abcdef, b1 = bcde1, b2 = bcde2)
}

str(f(1, 2, 3))
str(f(2, 3, abcdef = 1))
str(f(2, 3, a = 1))
#str(f(1, 3, b = 1)) # b nam nematchne # error: argument 3 matches multiple formal arguments

### Functional Delegation with … ###

### task 2

create_df<-function(a,b,c,...) {
    df<-rbind(a,b,c,...)
    df<-as.data.frame(df)
    df
    # je tu nejaky warning TODO
}

a <- 1:10 
b <- 2:11  
c <- 3:12

create_df(a,b,c) # fun without ...

x<- 1:4

create_df(a,b,c,x,a,c)

### Primitive Functions ###
# their formals(), body(), and environment() are all NULL

sum
formals(sum) # body(sum) ; environment(sum)
## NULL

is.primitive(sum)
is.function(sum)
is.function(create_df)

### Generic Functions ###
# summary()
## If your object is a data frame, summary() behaves one way.
## If your object is a linear regression model, summary() does something else.

### Anonymous Functions ###

#This is a named function: 
f <- function(x) { x + x} ; f(3) 
# This is an anonymous function: 
(function(x) x + x)(3) # volanie anonymnej funkcie je rovno na definicii
# alebo sa da anonymna funkcia zapisat takto
(\(x) x + x)(3)

###############
### TUT 3.2 ###
###############

### Function Composition ###
### Three Ways to Compose Functions
# 1
square <- function(x) x^2
deviation <- function(x) x - mean(x)
# 2 - python style
out <- deviation(data$skill_dribbling)
out <- square(out)
out <- mean(out)
out <- sqrt(out)
out
x <- data$skill_dribbling
sqrt(mean(square(deviation(x))))
# 3 - pipes
data$skill_dribbling |>
  deviation() |>
  square() |>
  mean() |>
  sqrt()

### The R Pipe Operator |> ###
#  |> f(y) is equivalent to f(x, y)

### The Magrittr Pipe Operator %>% ###
# y %>% f(x, .) is equivalent to f(x, y)
# z %>% f(x, y, arg = .) is equivalent to f(x, y, arg = z)

### Applications
# data wrangling
# data pivoting
# scientific plotting and ML model fitting (model fitting ggplot2)

###############
### TUT 3.3 ###
###############

### Data Wrangling ###

### Data Frames - long/short format (v poznamkach)

### Dplyr: Grammar of DF Manipulation ###

### Extracting Variables with select() ###

data %>%
select(short_name, dob,
            skill_moves, skill_dribbling, skill_curve, 
            skill_fk_accuracy, skill_long_passing, skill_ball_control) %>% 
print(n=3)

data %>% 
select(short_name, dob, starts_with("skill")) %>% 
        print(n=3)

data %>% 
  select(short_name, dob, where(is.numeric)) %>%
  print(n=3)

### Extracting Variables with . ###

typeof(data %>% .$skill_dribbling) # This code extracts numeric vector of dribbling values.

### Extracting Observations with filter() ###

data %>% 
  select(short_name, dob, starts_with("skill")) %>%
  filter(skill_dribbling > mean(data$skill_dribbling)) %>%
  print(n=5)

data %>% 
  filter(skill_dribbling > 80 & skill_dribbling < 90)

data %>%
  select(short_name, dob, starts_with("skill")) %>%
  filter(short_name %in% c("R. Lewandowski", "Cristiano Ronaldo", "Neymar Jr")) %>%
  print(n=3)

### Data Frame Operations ###

### Column Modification with mutate()
# creating new column
data %>%
  select(short_name, dob, starts_with("skill")) %>%
  filter(short_name %in% c("R. Lewandowski", "Cristiano Ronaldo", "Neymar Jr")) %>%
  filter(skill_dribbling > mean(data$skill_dribbling)) %>%
  mutate(normalized_SD = skill_dribbling - mean(data$skill_dribbling))
# modifying an existing column
data %>%
  select(short_name, dob, starts_with("skill")) %>%
  filter(short_name %in% c("R. Lewandowski", "Cristiano Ronaldo", "Neymar Jr")) %>%
  filter(skill_dribbling > mean(data$skill_dribbling)) %>%
  mutate(skill_dribbling = skill_dribbling - mean(data$skill_dribbling))

tmp <- data %>%
    select(short_name, value_eur) %>%
    filter(short_name == "L.Messi")
tmp %>% add_row(short_name="P.Belej",value_eur=100)

# modifying Multiple Columns with across()
data %>%
  select(short_name, dob, starts_with("skill")) %>%   
  filter(short_name %in% c("R. Lewandowski", "Cristiano Ronaldo", "Neymar Jr")) %>%
  mutate(across(where(is.numeric), as.integer))

# Renaming Columns with rename()
data %>%
  select(short_name, dob, starts_with("skill")) %>%
  filter(short_name %in% c("R. Lewandowski", "Cristiano Ronaldo", "Neymar Jr")) %>%
  rename(date_of_birth = dob)

# Rearrange Columns with relocate()
data %>%
  select(short_name, dob, starts_with("skill")) %>%
  filter(short_name %in% c("R. Lewandowski", "Cristiano Ronaldo", "Neymar Jr")) %>%
  relocate(starts_with("skill"), .after = short_name)
data %>%
  select(short_name, dob, starts_with("skill")) %>%
  filter(short_name %in% c("R. Lewandowski", "Cristiano Ronaldo", "Neymar Jr")) %>%
  relocate(where(is.numeric), .before = where(is.character))

# Insert Rows with add_rows()
tmp <- data %>% 
  select(short_name, value_eur) %>% 
  filter(short_name == "L. Messi")
tmp %>% add_row(short_name = "P. Belej", value_eur = 10000)

### Aggregate Summary Reports ###

# Summary Statistics with summarise()
data %>% 
  select(short_name, dob, skill_dribbling) %>%
  summarise(mean_dribbling = mean(skill_dribbling),
            min_dribbling = min(skill_dribbling),
            max_dribbling = max(skill_dribbling),
            number_of_records = n())

# Data Aggregation with group_by()
data %>% 
  select(short_name, dob, league_level, skill_dribbling) %>%
  group_by(league_level) %>% 
  summarise(mean_dribbling = mean(skill_dribbling),
            min_dribbling = min(skill_dribbling),
            max_dribbling = max(skill_dribbling),
            number_of_records = n())

# Ordering rows with arrange()
data %>% 
  select(short_name, dob, league_level, skill_dribbling) %>%
  arrange(desc(skill_dribbling))

###############
### TUT 3.4 ###
###############

### The Principles of Tidy Data ###

library(tidyverse)
library(magrittr)

#Set working directory, so you do not need to repeat long paths.
setwd("~/Elements of AI/Labs/Data/FIFA.22.Original.Data/")
data <- read_csv("players_22.csv", col_names = TRUE, num_threads = 4)
data %<>% select(short_name, dob, starts_with("skill")) %>%
   slice(1:5)
data

### Data Loading

### task 2
# read_csv() z tidyverse ma lepsie dekodovanie (formatovat?)
# base funkcia read.csv() to nie vzdy vie data spravne dekodovat 

data <- read_csv("players_22.csv", col_names = TRUE, num_threads = 4)
data %<>% select(short_name, dob, starts_with("skill")) #%>%
   #slice(1:5)
data

### Tidy data

# parovanie cez key=value (parovanie dat)

pivot_longer(data,              
             !c(short_name, dob), # These columns are not be touched.
             names_to = "skill",  # The key is made for all three skills...
             values_to = "value") # from which values are extracted.
# opacne
help("pivot_wider")

# Multiple variables are stored in one column

separate(data, dob, sep = "-", into = c("dob_year", "dob_month", "dob_day"))
# opacne
help("unit")