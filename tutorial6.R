library(dplyr)
library(ggplot2)

install.packages('caret')
library(caret)

titanic = read.csv('titanic.csv')

table(titanic$Survived)

titanic = titanic %>%
  mutate(PassengerId = as.character(PassengerId))

hist(as.numeric(titanic$Age))

titanic %>%
  ggplot(aes(x = Age)) +
  geom_histogram(binwidth = 5)

titanic = titanic %>%
  mutate(Age_cat = case_when(Age >= 50 ~ 'old',
                          Age >= 20 & Age <50 ~ 'mid',
                          Age < 20 ~ 'young',
                          TRUE ~ NA))

# create test & train datasets

## 75% of the sample size
smp_size = floor(0.75 * nrow(titanic))

## set the seed to make your partition reproducible
set.seed(42)
train_ind = sample(seq_len(nrow(titanic)), size = smp_size)

train <- titanic[train_ind, ]
test <- titanic[-train_ind, ]

table(train$Survived)
table(test$Survived)
# training a model
logistic_mod <- glm(Survived ~ Age_cat + Sex + Pclass,
                      data = train,
                      family = "binomial")
logistic_mod

# Summary
summary(logistic_mod)
# These coefficients in logistic regression represent the response variable’s log odds or logit.
# The standard errors related to the estimated coefficients are shown in the “Std. Error” column.

# making predictions
# type="response" produces a probability that each document is in each class
pred_survival <- predict(logistic_mod,
                       test, type = "response")
pred_survival


# Changing probabilities

pred_survival <- as.vector(ifelse(pred_survival > 0.5, 1, 0))
pred_survival
test$Survived


# raw accuracy
mean(pred_survival==test$Survived, na.rm = TRUE)

# Confusion matrix
table(pred_survival,test$Survived)

# easier to read
round(prop.table(table(pred_survival,test$Survived)),2)

results = confusionMatrix(as.factor(pred_survival), as.factor(test$Survived))

# precision = TP / (TP + FP)
precision = results[[3]][[1]]

# recall = TP / (TP + FN)
recall = results[[4]][[1]]

F1 = 2 * (precision * recall) / (precision + recall)
print(c(precision, recall, F1))

# Other types of machine learning algorithms

# 1. Simple regression
# lets take the auto dataset from the first week
auto = read.csv('auto.csv', header = TRUE) %>%
  select(-X)

## set the seed to make your partition reproducible
set.seed(42)
## 70% of the sample size
smp_size = floor(0.70 * nrow(auto))
train_ind = sample(seq_len(nrow(auto)), size = smp_size)

train <- auto[train_ind, ]
test <- auto[-train_ind, ]

mod_ols = lm(price ~ weight + foreign + mpg, data = train)
pred_ols = predict(mod_ols, test)

AvP = as.data.frame(cbind(Actual = test$price, Pred = pred_ols))

AvP %>%
  ggplot(aes(x = Actual, y = Pred)) +
  geom_point()


# 2. Random forest
install.packages('randomForest')
library(randomForest)
## 75% of the sample size
smp_size = floor(0.75 * nrow(titanic))

## set the seed to make your partition reproducible
set.seed(42)
train_ind = sample(seq_len(nrow(titanic)), size = smp_size)

train = titanic[train_ind, ]
test = titanic[-train_ind, ]

mod_rf = randomForest(Survived~., data=na.omit(train), proximity=TRUE)

pred_rf <- predict(mod_rf, test)
pred_rf <- as.vector(ifelse(pred_rf > 0.5, 1, 0))
results = confusionMatrix(as.factor(pred_rf), as.factor(test$Survived))

# raw accuracy
mean(pred_rf==test$Survived, na.rm = TRUE)

# Confusion matrix
table(pred_rf,test$Survived)

# precision = TP / (TP + FP)
precision = round(results[[3]][[1]],2)

# recall = TP / (TP + FN)
recall = round(results[[4]][[1]], 2)

F1 = round(2 * (precision * recall) / (precision + recall),2)
print(c(precision, recall, F1))

