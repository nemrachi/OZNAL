#' ---
#' title: Lab1
#' author: Ema Richnakova
#' ---

### UNDERSTANDING DATA

### task 1

library(tidyverse)
library(lobstr)

#setwd("./Elements of AI/Data/FIFA 22") # Set the correct path to players_22.csv
getwd() # Find where you are
# list.files() # List all files in the working directory

data<-read_csv("players_22.csv",col_names=TRUE,num_threads=4) # Read data into R
# data # Check that data are properly loaded 

### task 2

#View(data) # Launch an interactive R viewer
class(data) # Get R class hierarchy for your data
typeof(data) # Get an internal representation of data in R
dim(data) # Get table dimensions
lobstr::obj_size(data) # Get size of loaded data

### task 3 (bonus) count the # of different column types

# sapply() -> returns vector of data types for each column
# table() to count the number of occurrences of each unique data type
tas3<-table(sapply(data,class))
tas3

### task 4
# Enumerate the number of complete and incomplete cases in players_22.csv. 
# Fill in your code and explain the problem of incomplete cases.

tas4<-sum(complete.cases(data))
tas4

### task 5
# Write a R program that counts NA in a given column. 
# For example, the overall column does not have a single NA 
# value, but nation_position does have a lot of them

tas5<-table(is.na(data$overall))
tas5
tas51<-table(is.na(data$nation_position))
tas51

### DATA SLICING

### task 6
# Explain why statement data[1:120] returns an error? 
# How does it differ from the similar statement data[1:120, ]?

# data[1:120] -> 1 parameter only for rows, but R expects either row and col indices
# data[1:120, ] -> 2 parameters -> rows, cols

### task 7

data[data$player_positions=="GK", ]
data[1:4, ]
data[data$overall <= 50, ]
data[data$overall <= 50 | data$overall >= 90, ]

### task 8

# !!!!!!!!!!!!!!!!!!!!!!!! #
head(data[["short_name"]]) # print first 6 lines
head(data[["short_name"]][1]) # tail - for last
#glimpse(head(data[["wage_eur"]]))
head(data["wage_eur"]) # single [] -> col (+ tibble dim info), double [[]] -> row !!!

### COLUMN TRANSFORMATION

### task 9

# Step 1: Normalize
data$rank_normalized <- data$overall - data$potential # Fill in your code that will do the substraction
# Step 2: Choose only relevant columns (short_name, overall, potential and rank_normalized)
data_slice <- data[,c("short_name","overall","potential","rank_normalized")] # Restrict data's columns 
# Step 3: Print the first five lines
head(data_slice,5)

### task 10
# Log-transform player’s wages.
# Instead of creating a new column, replace the original values with the logged ones.

data$wage_eur<-log(data$wage_eur)
head(data[["wage_eur"]],5)