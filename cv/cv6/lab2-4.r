#' ---
#' title: Lab2.4
#' author: Ema Richnakova
#' ---

#'# Sampling
#' 
#'## Simple 
#'
library(tidyverse)
#setwd("./cv/cv6")
data <- read_csv("income.data.csv", col_names = TRUE, num_threads = 4)
data