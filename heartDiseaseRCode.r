## code by Logan Prasse and Andrew Diaz

library(readr)
data <- read_csv("https://www.kaggle.com/ronitf/heart-disease-uci/download")

## Data Cleaning
data$age <- as.numeric(data$age)
data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$trestbps <- as.numeric(data$trestbps)
data$chol <- as.numeric(data$chol)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$thalach <- as.numeric(data$thalach)
data$exang <- as.factor(data$exang)
data$oldpeak <- as.numeric(data$oldpeak)
data$slope <- as.factor(data$slope)
data$ca <- as.numeric(data$ca)
data$thal <- as.factor(data$thal)
data$target <- as.factor(data$target)

data <- as.data.frame(data)

View(data)

library('ggplot2')
ggplot(data, aes(y=chol)) + geom_boxplot()
data <- data[-86,]
ggplot(data, aes(y=oldpeak)) + geom_boxplot()
##decided to keep
ggplot(data, aes(x=thal)) + geom_bar()
data <- data[c(-49, -282),]


## Exploratory Analysis/Visualization
library(ggplot2)

ggplot(data, aes(x=chol, fill = target)) + geom_bar()

ggplot(data, aes(x=age, fill=target)) + geom_histogram()

ggplot(data, aes(x=sex, fill=target)) + geom_bar()

ggplot(data, aes(x=chol, y=sex, col=target)) + geom_point()

ggplot(data, aes(x=ca, y=oldpeak, col = target)) + geom_point()

ggplot(data, aes(x=age, y=chol, col=target)) + geom_point()

ggplot(data, aes(x=ca, fill = target)) + geom_bar()

ggplot(data, aes(x=age, fill = cp)) + geom_histogram()

ggplot(data, aes(x=target, fill = cp)) + geom_bar()

ggplot(data, aes(x=trestbps, fill = target)) + geom_histogram()

str(data)



chol_with <- mean(data$chol[data$target=='1'])
chol_with

chol_without <- mean(data$chol[data$target=='0'])
chol_without

age_with <- mean(data$age[data$target=='1'])
age_with

age_without <- mean(data$age[data$target=='0'])
age_without


## Begin Model Building
set.seed(123)
train_rows <- sample(x = 1:nrow(data), size = nrow(data)*0.8)
x_train <- data[train_rows , ]
x_test <- data[-train_rows , ]

#Regression Analysis
model1 <- lm(as.numeric(target) ~ . , data = x_train)
summary(model1)
AIC(model1)

# Stepwise Regression
library(MASS)

model2 <- stepAIC(model1, direction = "both")
summary(model2)
AIC(model2)


predicted_dependent <- predict(object = model1, newdata = x_test)
predicted_dependent 

predicted_dependent2 <- predict(object = model2, newdata = x_test)
predicted_dependent2

library(Metrics)

Metrics::mape(actual = as.numeric(x_test$target), predicted_dependent)
Metrics::rmse(actual = as.numeric(x_test$target), predicted_dependent)
Metrics::mape(actual = as.numeric(x_test$target), predicted_dependent2)
Metrics::rmse(actual = as.numeric(x_test$target), predicted_dependent2)


## Decision Tree Models


for (i in 1:ncol(data)){
  print(names(data)[i])
  print(unique(data[,i]))
}

# logistic regression
fit_glm <- glm((as.numeric(target)-1) ~ . , data = x_train)
summary(fit_glm)

# Gini tree
library(rpart)
fit_gini <- rpart(target ~ ., data = x_train)
fit_gini

# con tree
library(party)
fit_con <- ctree(target ~ ., data = x_train,controls = ctree_control(maxdepth = 10))
fit_con

# dev tree
library(tree)
fit_dev <- tree(target ~ ., data = x_train, split = "deviance")
fit_dev

library(ROCR)
predicted_lr <- predict.glm(object = fit_glm, newdata = x_test)
predicted_lr

predicted_gini <- predict(object = fit_gini, newdata = x_test)
predicted_gini <- predicted_gini[,2]
predicted_gini

predicted_con <- predict(object = fit_con, newdata = x_test, type = "prob")
predicted_con <- unlist(lapply(predicted_con, '[[', 2))
predicted_con

predicted_dev <- predict(object = fit_dev, newdata = x_test)
predicted_dev <- predicted_dev[,2]
predicted_dev


library(pROC)
auc(x_test$target , as.numeric(predicted_lr))
auc(x_test$target , as.numeric(predicted_gini))
auc(x_test$target , as.numeric(predicted_con))
auc(x_test$target , as.numeric(predicted_dev))

##Confusion Matrices

library(caret)
library(e1071)

predicted_lr[predicted_lr >= 0.5] <- 1 
predicted_lr[predicted_lr < 0.5] <- 0
predicted_lr
confusionMatrix(data = as.factor(predicted_lr), 
                reference = as.factor(as.numeric(x_test$target)-1))
gg(m1$table)

predicted_gini[predicted_gini >= 0.5] <- 1 
predicted_gini[predicted_gini < 0.5] <- 0
predicted_gini
confusionMatrix(data = as.factor(predicted_gini), 
                reference = as.factor(as.numeric(x_test$target)-1))

predicted_con[predicted_con >= 0.5] <- 1 
predicted_con[predicted_con < 0.5] <- 0
predicted_con
confusionMatrix(data = as.factor(predicted_con), 
                reference = as.factor(as.numeric(x_test$target)-1))

predicted_dev[predicted_dev >= 0.5] <- 1 
predicted_dev[predicted_dev < 0.5] <- 0
predicted_dev
confusionMatrix(data = as.factor(predicted_dev), 
                reference = as.factor(as.numeric(x_test$target)-1))
