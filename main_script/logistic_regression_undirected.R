##########################################
## Logistic Regression
## Mattia Girardi
## 02.11.2020
#########################################

# set working directory
setwd("~/Desktop/Bachelor Thesis/code/bachelor_thesis")

# install packages
list.of.packages <- c("data.table", "dplyr")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

#### load in master
master_measures_2 <- fread("output/undirected/master_measures_2.csv")
summary(master_measures_2)

#### transform Social,Offline and Social,Online into logical variables
for(i in 1:length(master_measures_2$ID)){
  if(master_measures_2[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master_measures_2[i, "NetworkDomain"] <- gsub(".*", "1", master_measures_2[i, "NetworkDomain"])
  } else {
    master_measures_2[i, "NetworkDomain"] <- gsub(".*", "0", master_measures_2[i, "NetworkDomain"])
  }
}

master_measures_2$NetworkDomain <- as.factor(master_measures_2$NetworkDomain)

#### logistic regression model
network_glm_2 <- glm(NetworkDomain ~ AverageDegree + AveragePathLength + AverageTransitivity + Closeness +
                    DegreeAssortativity + DegreeDistribution + Density + EigenvectorCentrality + GlobalTransitivity,
                   family = binomial, data = master_measures_2)

summary(network_glm_2)

glm.probs <- predict(network_glm_2,type = "response")
mean(glm.probs)
# [1] 0.4043716

glm.pred <- ifelse(glm.probs > 0.5, "TRUE", "FALSE")

master_measures_2 <- na.omit(master_measures_2)
pred <- table(glm.pred, master_measures_2$NetworkDomain)
(pred[1] + pred[4])/length(master_measures_2$NetworkDomain)
# [1] 0.7814208

#### caret package
# ensure results are repeatable
set.seed(7)
# load the library
install.packages("mlbench")
install.packages("caret")
install.packages("e1071")
library(e1071)
library(mlbench)
library(caret)

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=10)
# train the model
model <- train(NetworkDomain ~ AverageDegree + AveragePathLength + AverageTransitivity + Closeness +
                 DegreeAssortativity + DegreeDistribution + Density + EigenvectorCentrality + GlobalTransitivity
               , data=master_measures_2, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)


#### train classifier
library(class)
# load in master
master_measures_2 <- fread("output/undirected/master_measures_2.csv")

# transform Social,Offline and Social,Online into logical variables
for(i in 1:length(master_measures_2$ID)){
  if(master_measures_2[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master_measures_2[i, "NetworkDomain"] <- gsub(".*", "TRUE", master_measures_2[i, "NetworkDomain"])
  } else {
    master_measures_2[i, "NetworkDomain"] <- gsub(".*", "FALSE", master_measures_2[i, "NetworkDomain"])
  }
}

master_measures_2$NetworkDomain <- as.logical(master_measures_2$NetworkDomain)

master_measures_2 <- na.omit(master_measures_2)

set.seed(1234)
ind <- sample(2, nrow(master_measures_2), replace=TRUE, prob=c(0.95, 0.33))

master_training <- master_measures_2[ind == 1, 5:13]
master_test <- master_measures_2[ind == 2, 5:13]

master_trainLabels <- master_measures_2[ind==1,4]
master_testLabels <- master_measures_2[ind==2,4]

master_pred <- knn(train = master_training, test = master_test, cl = master_trainLabels$NetworkDomain, k = 3)

master_testLabels <- data.frame(master_testLabels)

merge <- data.frame(master_testLabels, master_pred)

pred <- table(merge)
(pred[1]+pred[4])/length(master_test$AverageDegree)
# [1] 0.7449664

################################################################
#### use of caret package
library(caret)
master_measures_2 <- fread("output/undirected/master_measures_2.csv")
master_measures_2[master_measures_2 == 0 | master_measures_2 == 1,] <- NA
master_measures_2[1, 1] <- 1

for(i in 1:length(master_measures_2$ID)){
  if(master_measures_2[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master_measures_2[i, "NetworkDomain"] <- gsub(".*", "1", master_measures_2[i, "NetworkDomain"])
  } else {
    master_measures_2[i, "NetworkDomain"] <- gsub(".*", "0", master_measures_2[i, "NetworkDomain"])
  }
}

master_measures_2$NetworkDomain <- as.factor(master_measures_2$NetworkDomain)
master_measures_2 <- na.omit(master_measures_2)

## logistic regression
default_glm_mod <- train(
  form = NetworkDomain ~ AverageDegree + AveragePathLength + AverageTransitivity + Closeness +
    DegreeAssortativity + DegreeDistribution + Density + EigenvectorCentrality + GlobalTransitivity + number_edges,
  data = master_measures_2,
  trControl = trainControl(method = "cv", number = 5),
  method = "glm",
  family = "binomial"
)
summary(default_glm_mod)

## train classifier with 100 and 500 iterations
# logistic regression; all variables; only undirected networks
set.seed(1234)
index <- createDataPartition(master_measures_2$NetworkDomain, times = 100, p=0.95, list=FALSE)

accuracy <- c()
for(i in 1:100){
  master_training <- master_measures_2[index[, i],]
  master_test <- master_measures_2[-index[, i],]
  default_glm_mod <- train(
    form = NetworkDomain ~ AverageDegree + AveragePathLength + AverageTransitivity + Closeness +
      DegreeAssortativity + DegreeDistribution + Density + EigenvectorCentrality + GlobalTransitivity + number_edges,
    data = master_training,
    trControl = trainControl(method = "cv", number = 5),
    method = "glm",
    family = "binomial"
  )
  prediction <- predict(default_glm_mod, newdata = master_test)
  result <- table(master_test$NetworkDomain, prediction)
  accuracy <- combine(accuracy, (result[1] + result[4])/(27))
}
sd(accuracy)
mean(accuracy)

## 100 iterations
# > 50 warnings
# SD : [1] 0.08127631
# mean : [1] 0.7685185

## 500 iterations
# > 50 warnings
# SD : [1] 0.07934681
# mean : [1] 0.7635556

# logistic regression; significant variables; only undirected networks
set.seed(1234)
index <- createDataPartition(master_measures_2$NetworkDomain, times = 500, p=0.95, list=FALSE)

accuracy <- c()
for(i in 1:500){
master_training <- master_measures_2[index[, i],]
master_test <- master_measures_2[-index[, i],]
default_glm_mod <- train(
  form = NetworkDomain ~ AveragePathLength + DegreeAssortativity + DegreeDistribution + number_edges,
  data = master_training,
  trControl = trainControl(method = "cv", number = 5),
  method = "glm",
  family = "binomial"
)
prediction <- predict(default_glm_mod, newdata = master_test)
result <- table(master_test$NetworkDomain, prediction)
accuracy <- combine(accuracy, (result[1] + result[4])/27)
}
sd(accuracy)
mean(accuracy)

## 100 iterations
# 2 errors
# SD : [1] 0.07781429
# mean : [1] 0.7888889

## 500 iterations
# 14 errors
# SD : [1] 0.0768635
# mean : [1] 0.7843704









