# Clear packages
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear Environment
rm(list = ls())

# Clear console
cat("\014")  # ctrl+L

#installing/importing libraries
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

#loading titanic dataset
titanic_data <- "https://goo.gl/At238b" %>%  
  read.csv %>% 
  #get only survived, embarked, sex, num of siblings/spouses on board,
  #number of parents/children on board, and fare features
  select(survived, embarked, sex, 
         sibsp, parch, fare) %>%
  #then turn embarked and sex features into factors
  mutate(embarked = factor(embarked),
         sex = factor(sex))

#display the object structure
str(titanic_data)

#alternative code to see missing values
#apply(is.na(titanic_data), 2, which)

#3 missing values, 1 in fare and 2 in embarked
titanic_data[!complete.cases(titanic_data),]

#replace a missing value with mean in fare feature
titanic_data$fare[is.na(titanic_data$fare)] <- mean(titanic_data$fare, na.rm = TRUE)

#displays categorical representation of data (values and its freq)
table(titanic_data$embarked)

#mode function to replace missing values
calc_mode <- function(x){
  
  #List the distinct / unique values
  distinct_values <- unique(x)
  
  #Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  
  #Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}

#apply mode function to replace missing values
titanic_data$embarked[is.na(titanic_data$embarked)] <- calc_mode(titanic_data$embarked) 

#check for missing values
titanic_data[!complete.cases(titanic_data),]

#initialise a pseudorandom number
set.seed(123)

#train test split
sample_data = sample.split(titanic_data, SplitRatio = 0.75)
train_data <- subset(titanic_data, sample_data == TRUE)
test_data <- subset(titanic_data, sample_data == FALSE)

#plot decision tree
rtree <- rpart(survived ~ ., train_data)
rpart.plot(rtree)

#plot conditional parting plot
ctree_ <- ctree(survived ~ ., train_data)
plot(ctree_)