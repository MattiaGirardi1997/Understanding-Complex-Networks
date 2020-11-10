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
master_measures <- fread("output/master_measures.csv")

#### transform Social,Offline and Social,Online into logical variables
for(i in 1:length(master_measures$ID)){
  if(master_measures[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master_measures[i, "NetworkDomain"] <- gsub(".*", "1", master_measures[i, "NetworkDomain"])
  } else {
    master_measures[i, "NetworkDomain"] <- gsub(".*", "0", master_measures[i, "NetworkDomain"])
  }
}

master_measures$NetworkDomain <- as.factor(master_measures$NetworkDomain)

#### logistic regression model
network_glm <- glm(NetworkDomain ~ AverageDegree + AveragePathLength + AverageTransitivity + Betweenness + Closeness +
                    DegreeAssortativity + DegreeDistribution + Density + EigenvectorCentrality + GlobalTransitivity, family = binomial, data = master_measures)

summary(network_glm)

glm.probs <- predict(network_glm,type = "response")

mean(glm.probs)
# [1] 0.4634656

glm.pred <- ifelse(glm.probs > 0.5, "TRUE", "FALSE")

master_measures <- na.omit(master_measures)
pred <- table(glm.pred, master_measures$NetworkDomain)
(pred[1] + pred[4])/length(master_measures$NetworkDomain)
# [1] 0.782881

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
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(NetworkDomain ~ AverageDegree + AveragePathLength + AverageTransitivity + Betweenness + Closeness +
                 DegreeAssortativity + DegreeDistribution + Density + EigenvectorCentrality + GlobalTransitivity
               , data=master_measures, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)


#### train classifier
library(class)
#### load in master
master_measures <- fread("output/master_measures.csv")

#### transform Social,Offline and Social,Online into logical variables
for(i in 1:length(master_measures$ID)){
  if(master_measures[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master_measures[i, "NetworkDomain"] <- gsub(".*", "TRUE", master_measures[i, "NetworkDomain"])
  } else {
    master_measures[i, "NetworkDomain"] <- gsub(".*", "FALSE", master_measures[i, "NetworkDomain"])
  }
}

master_measures$NetworkDomain <- as.logical(master_measures$NetworkDomain)

master_measures <- na.omit(master_measures)

set.seed(1234)
ind <- sample(2, nrow(master_measures), replace=TRUE, prob=c(0.67, 0.33))

master_training <- master_measures[ind == 1, 4:14]
master_test <- master_measures[ind == 2, 4:14]

master_trainLabels <- master_measures[ind==1,3]
master_testLabels <- master_measures[ind==2,3]

master_pred <- knn(train = master_training, test = master_test, cl = master_trainLabels$NetworkDomain, k = 3)

master_testLabels <- data.frame(master_testLabels)

merge <- data.frame(master_testLabels, master_pred)

pred <- table(merge)
(pred[1]+pred[4])/length(master_test$AverageDegree)
# [1] 0.5816993

### use of caret package
library(caret)
master_measures <- fread("output/master_measures.csv")[, -c("Reciprocity")]
master_measures[master_measures == 0 | master_measures == 1,] <- NA

for(i in 1:length(master_measures$ID)){
  if(master_measures[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master_measures[i, "NetworkDomain"] <- gsub(".*", "1", master_measures[i, "NetworkDomain"])
  } else {
    master_measures[i, "NetworkDomain"] <- gsub(".*", "0", master_measures[i, "NetworkDomain"])
  }
}

master_measures$NetworkDomain <- as.factor(master_measures$NetworkDomain)
master_measures <- na.omit(master_measures)

set.seed(1234)
index <- createDataPartition(master_measures$NetworkDomain, times = 500, p=0.95, list=FALSE)

accuracy <- c()
for(i in 1:500){
  master_training <- master_measures[index[, i],]
  master_test <- master_measures[-index[, i],]
  default_glm_mod <- train(
    form = NetworkDomain ~ AverageDegree + AveragePathLength + AverageTransitivity + Betweenness + Closeness +
      DegreeAssortativity + DegreeDistribution + Density + EigenvectorCentrality + GlobalTransitivity,
    data = master_training,
    trControl = trainControl(method = "cv", number = 5),
    method = "glm",
    family = "binomial"
  )
  prediction <- predict(default_glm_mod, newdata = master_test)
  result <- table(master_test$NetworkDomain, prediction)
  accuracy <- combine(accuracy, (result[1] + result[4])/(22))
}
mean(accuracy)
## 100 iterations
# > 50 warnings
# [1] 0.7627273

## 500 iterationa
# > 50 warnings
# [1] 0.7764545

accuracy <- c()
for(i in 1:500){
master_training <- master_measures[index[, i],]
master_test <- master_measures[-index[, i],]
default_glm_mod <- train(
  form = NetworkDomain ~ AverageDegree + DegreeAssortativity + DegreeDistribution + Density + 
    EigenvectorCentrality + GlobalTransitivity,
  data = master_training,
  trControl = trainControl(method = "cv", number = 5),
  method = "glm",
  family = "binomial"
)
prediction <- predict(default_glm_mod, newdata = master_test)
result <- table(master_test$NetworkDomain, prediction)
accuracy <- combine(accuracy, (result[1] + result[4])/22)
}
mean(accuracy)
## 100 iterations
# 7 errors
# [1] 0.7472727

## 500 iterations
# > 50 errors
# [1] 0.7610909



summary(default_glm_mod)



for(i in 1:100){
  master_training <- master_measures[index[, i],]
  master_test <- master_measures[-index[, i],]
  default_glm_mod <- glm(data = master_training, NetworkDomain ~ AverageDegree + AveragePathLength + AverageTransitivity + Betweenness + Closeness +
                           DegreeAssortativity + DegreeDistribution + Density + EigenvectorCentrality + GlobalTransitivity,
                         family = binomial)
  prediction <- predict(default_glm_mod, newdata = master_test[,3:13])
  result <- table(master_test$NetworkDomain, prediction)
  accuracy <- combine(accuracy, (result[1] + result[4])/29)
}


