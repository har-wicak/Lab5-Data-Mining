# Clear packages
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear Environment
rm(list = ls())

# Clear console
cat("\014")  # ctrl+L

install.packages("party")
library(rpart)
library(readr)
library(caTools)
library(dplyr)
library(party)
library(partykit)
library(rpart.plot)
library("caret")
library("e1071")

titanic_data <- "https://goo.gl/At238b" %>%  
  read.csv %>% 
  select(survived, embarked, sex, 
         sibsp, parch, fare) %>%
  mutate(embarked = factor(embarked),
         sex = factor(sex))

str(titanic_data)
#apply(is.na(titanic_data), 2, which)
titanic_data[!complete.cases(titanic_data),]
titanic_data$fare[is.na(titanic_data$fare)] <- mean(titanic_data$fare, na.rm = TRUE)
table(titanic_data$embarked)

calc_mode <- function(x){
  
  # List the distinct / unique values
  distinct_values <- unique(x)
  
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}
titanic_data$embarked[is.na(titanic_data$embarked)] <- calc_mode(titanic_data$embarked) 
titanic_data[!complete.cases(titanic_data),]

set.seed(123)
sample_data = sample.split(titanic_data, SplitRatio = 0.75)
train_data <- subset(titanic_data, sample_data == TRUE)
test_data <- subset(titanic_data, sample_data == FALSE)

rtree <- rpart(survived ~ ., train_data)
rpart.plot(rtree)

ctree_ <- ctree(survived ~ ., train_data)
plot(ctree_)