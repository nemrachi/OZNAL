# tut

x <- 1:8
x

attributes(x)
colnames(x)

# 2D array

dim(x) <- c(2,4) # Folds x into an array that has 2 rows and 4 columns # 2d array
x # Prints x
dimnames(x) <- list(c("Alpha", "Beta"),
                    c("Gamma", "Delta", "Epsilon","Zeta")) # name cols and rows
x
dim(x) <- c(4,2) ; x

# Matrix

set.seed(123)
m <- matrix(nrow = 0, ncol = 3, byrow = TRUE) # Initializes empty matrix.
for (i in 1:3) m <- rbind(m, rnorm(n = 3, mean = i)) # Fills it with randomly generated numbers.
# r(row)bind / c(ol)bind
colnames(m) <- c("Alpha", "Beta", "Gamma") # Annotates cols and rows.
rownames(m) <- c("Alpha", "Beta", "Gamma") ; m # Prints matrix.
m[1,]

help("array") # like man command

# Predicate and Logical Subsetting

m[c(TRUE, TRUE, FALSE), 1:2]
m[m<2] # Subset rows in which all values are less that 2.

# **Vectors can "flatten" elements

list1 <- list(list(1, 2), c(3, 4))
list1
list2 <- c(list(1, 2), c(3, 4))
list2

str(list1)