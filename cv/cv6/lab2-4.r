#' ---
#' title: Lab2.4
#' author: Ema Richnakova
#' ---

suppressMessages(library(tidyverse))
suppressMessages(library(magrittr))

#'# Sampling
#' 
#'## Simple sampling

#'### Task 1
#' Load the dataset and explore it. How data look like? Is it necessary to 
#' perform any adjustment to make the linear regression model?
#' 
#' * data are linear, no adjustmnets are needed

#setwd("cv/cv6/")
inc <- read_csv("income.data.csv", col_names = TRUE, num_threads = 4)
head(inc)

sample <- sample(c(TRUE, FALSE), nrow(inc), replace=TRUE, prob=c(0.7,0.3))
train <- inc[sample, ]
test <- inc[!sample, ]

model_lm <- lm(formula=happiness ~ ., data=train)
summary(model_lm)

#'### Task 2
#' Perform the modelling and compare the RMSE for training and testing dataset.
#' What is the outcome?

suppressMessages(library(caret)) # confusion matrix + RMSE

R2model <- sum(model_lm$residuals^2)
RMSEmodel <- sqrt(R2model / length(model_lm$residuals))
paste("The RMSE for training dataset is: ", RMSEmodel)

predicted_test<-predict(model_lm,test)
RMSE_test <- RMSE(predicted_test, test$happiness)
print(paste("The RMSE for testing dataset is:", RMSE_test))

#'## K-fold cross-validation
#' **Simple K-Folds** — We split our data into K parts. We split it into ten parts,
#' part 1 to part 10. We then build 10 different models, each model is trained
#' on 9 parts and tested on the remaining.
#' 
#'### Task 3
#' Create 10 folds using function createFolds() from the caret library. On each
#' fold, perform linear regression and compare the RMSE results. What does it
#' tell about your model and your data?

# Create indices for 10-fold cross-validation
folds <- createFolds(y = 1:nrow(inc), k = 10, list = TRUE, returnTrain = FALSE)
perform_linear_regression <- function(data, folds) {
  results <- list()
  for (i in 1:length(folds)) {
    # Get the indices for the current fold
    fold_indices <- unlist(folds[-i])
    # Split the data into training and testing sets
    train_data <- data[fold_indices, ]
    test_data <- data[folds[[i]], ]
    # Fit the linear regression model
    lm_model <- lm(happiness ~ ., data = train_data)
    # Predict on the test data
    predicted <- predict(lm_model, newdata = test_data)
    # Calculate RMSE
    rmse <- sqrt(mean((test_data$happiness - predicted)^2))
    # Store the results
    results[[i]] <- list(
      lm_model = lm_model,
      predicted = predicted,
      actual = test_data$happiness,
      rmse = rmse
    )
  }
  return(results)
}
# Call the function with your data and folds
results <- perform_linear_regression(inc, folds)
# Print RMSE for each fold
for (i in 1:length(results)) {
    cat("Fold", i, "RMSE:", results[[i]]$rmse, "\n")
}

#' **Why is the cross-validation important?**  
#' helps us better use our data, and it gives us much more information about our
#' algorithm performance. In complex machine learning models, it’s sometimes
#' easy not pay enough attention and use the same data in different steps of the
#' pipeline. This may lead to good but not real performance in most cases, or,
#' introduce strange side effects in others. We have to pay attention that we’re
#' confident in our models. Cross-Validation helps us when we’re dealing with
#' non-trivial challenges in our Data Science projects.

#' 
#'# ROC curves
#' **linear discriminant anakysis (LDA)**
#' 
#' * prepare data for LDA
#' * apply LDA onto dataset a visualize the outcome
#' * learn how to estimate the best cut-odd for classification methods
#' automatically by researching receiver operating characteristics, ROC.
#' 
#' LDA shines when there is substantial separation (aka clear water) between 2
#' classes, because parameter estimates for the logistic regression model become
#' unstable.

library(MASS) # LDA library

#' 
#'### Task 1
#' Load the credit dataset Default.csv (you will find it on the server). Think
#' about the dataset. Does balance values fulfills all requirement for LDA?
#' Which test would you chose to compare the distributions of balance values
#' between defaulters and non-defaulters? How would you modify the data to be
#' ready for using in your LDA model?
#' 
#' * one-hot encoding is needed for Yes/No

data <- read_csv("Default.csv", col_names = TRUE, num_threads = 4)
head(data)
#' One-hot encoding (**we can not use Yes/No**)
# default
data$default <- ifelse(data$default == "Yes", 1, 0)
# student
data$student <- ifelse(data$student == "Yes", 1, 0)
# Converting to binary variables to factors
data %<>% mutate(default = as_factor(default))
data %<>% mutate(student = as_factor(student))
data

#'### Task 2
#' Analyze the data you want to classify? How does the defaulters and the
#' non-defaulters groups look like? What inferences do you draw from your
#' analysis?

par(mfrow=c(1,2))
boxplot(balance ~ default, data = data)
boxplot(income ~ default, data = data)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
# QQ plot for "balance" variable by default status
qqnorm(data$balance[data$default == 1], main = "Defaulters - Balance QQ Plot")
qqline(data$balance[data$default == 1], col = "red")
qqnorm(data$balance[data$default == 0], main = "Non-Defaulters - Balance QQ Plot")
qqline(data$balance[data$default == 0], col = "blue")
# QQ plot for "income" variable by default status
qqnorm(data$income[data$default == 1], main = "Defaulters - Income QQ Plot")
qqline(data$income[data$default == 1], col = "red")
qqnorm(data$income[data$default == 0], main = "Non-Defaulters - Income QQ Plot")
qqline(data$income[data$default == 0], col = "blue")
par(mfrow=c(1,1))  # Reset plotting parameters

#' Analyses show an enrichment of clients with 0 account balance. They **will
#' impact LDA**, but **not logistic regression**, because **LDA uses central
#' tendencies and standard deviation for calculations**, and these are likely to be
#' skewed by disproportionately distributed data. **LDA is sensitive for data that
#' are not only being normally distributed, but also expects that observations
#' in both groups have exactly the same shape of the bell curve**. In other words,
#' they have the same variance. This is not the case for default data and this
#' would be place to acknowledge it.

#'### Task 3
#' Create the LDA model and make the prediction object. Fill-in the missing
#' parts of the code.

model_lda <- lda(default ~ balance, data = data)
lda_predict <- predict(object = model_lda)

#'### Task 4
#' Analyze the structure of the prediction object. What does it tell you?
#' 
#' * this

model_lda
str(lda_predict)
head(lda_predict$class)
head(lda_predict$posterior) # Extract sigma hat values for each class

#'## Receiver operating characteristics
#' Default cut-off for logistic regression might not be best cut-off possible  
#' comparing 0.5 cut-off to LDA doesn't make sense. Find ideal cut-off with ROC
#' (Receiver Operating Characteristics).  
#' ROC curve represents the optimal trade-off between True Positive Rate (TRP)
#' and False Positive Rate (FPR).
#' 
#'#### Cut-off
#' Threshold used to classify observations into different groups or classes
#' based on their predicted discriminant scores.

library(ROCit) # ROC curves library
model_glm <- glm(default ~ balance, data = data, family = binomial)

roc <- rocit(class = model_glm$y,
             score = model_glm$fitted.values)

par(mfrow = c(1,2))
plot(roc)
ksplot(roc)
par(mfrow = c(1,1))
summary(roc)

#' **AUC (Area Under Curve)**
#' 
#' * higher better
#' * classification-threshold-invariant
#' * best way how to tell which classification method is better (comparing ROC 
#' of logistic regression and LDA)
#' 
#' even better library than ROCit for data mining is **pROC**
#' 
#'## pROC
#' best for data mining

library(pROC) # ROC calculation
pROC <- pROC::roc(data$default, data$balance, algorithm = 2)
pROC_results <- coords(pROC, "local maximas", ret=c("threshold", "sens", "spec",
                                          "ppv", "npv", "youden", "closest.topleft"))
head(pROC_results)
tail(pROC_results)

#'### Task 5
#' Investigate data frame that you got from pROC. Why are there only 219 rows?
#' 
#' It seems that pROC returns only results for the most bend part of the ROC
#' curve and drops all other results. In this case, it would be for balances
#' from 652 USD to 2420 USD.

#'### Task 6
#' Think how can the resulting data frame be used to find the maximal value for
#' the “top left” or Youden and how would you pair that information with the
#' information about balance values and probabilities you have. This should be
#' relatively easy and doable with the knowledge that you already have.

max_youden <- max(pROC_results$youden) ; max_youden

#'## cutpointr
#' easiest way to get cutoff point  
#' gives you the exact optimal cutpoint along with the multitude of other
#' measurements

suppressMessages(library(cutpointr)) # optimal cutoff point
cp <- cutpointr(data, balance, default, method = maximize_metric, metric = sum_sens_spec)
summary(cp)
plot(cp)

#'### Task 7
#' Think about the optimal cutoff point. What does it tell you? Is the optimal
#' cutoff always optimal? How does it go with your goals?