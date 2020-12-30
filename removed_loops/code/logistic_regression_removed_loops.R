##########################################
## Logistic Regression removed_loops
## Mattia Girardi
## 28.12.2020
#########################################

# set working directory
setwd("~/Desktop/Bachelor Thesis/code/bachelor_thesis")

# install packages
list.of.packages <- c("data.table", "dplyr")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

#### load in master
master <- fread("removed_loops/output/removed_loops_measures.csv")
summary(master)

#### transform Social,Offline and Social,Online into logical variables
for(i in 1:length(master$ID)){
  if(master[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master[i, "NetworkDomain"] <- gsub(".*", "1", master[i, "NetworkDomain"])
  } else {
    master[i, "NetworkDomain"] <- gsub(".*", "0", master[i, "NetworkDomain"])
  }
}

master$NetworkDomain <- as.factor(master$NetworkDomain)

#### logistic regression model
network_glm_2 <- glm(NetworkDomain ~ AverageDegree + AveragePathLength + AverageTransitivity + 
                       BetweennessCentrality + Closeness + ClosenessCentrality +
                       DegreeAssortativity + DegreeCentrality + DegreeDistribution +
                       Density + EigenvectorCentrality + EigenvectorCentrality_2 +
                       GlobalTransitivity,
                     family = binomial, data = master)

summary(network_glm_2)

glm.probs <- predict(network_glm_2,type = "response")
mean(glm.probs)
# [1] 0.4198312

glm.pred <- ifelse(glm.probs > 0.5, "TRUE", "FALSE")

master <- na.omit(master)
pred <- table(glm.pred, master$NetworkDomain)
(pred[1] + pred[4])/length(master$NetworkDomain)
# [1] 0.7932489

########################################################################################
#### train classifier
library(class)
# load in master
master <- fread("removed_loops/output/removed_loops_measures.csv")

# transform Social,Offline and Social,Online into logical variables
for(i in 1:length(master$ID)){
  if(master[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master[i, "NetworkDomain"] <- gsub(".*", "TRUE", master[i, "NetworkDomain"])
  } else {
    master[i, "NetworkDomain"] <- gsub(".*", "FALSE", master[i, "NetworkDomain"])
  }
}

master$NetworkDomain <- as.logical(master$NetworkDomain)
master <- na.omit(master)

set.seed(1234)
ind <- sample(2, nrow(master), replace=TRUE, prob=c(0.95, 0.33))

master_training <- master[ind == 1, 6:18]
master_test <- master[ind == 2, 6:18]

master_trainLabels <- master[ind==1,3]
master_testLabels <- master[ind==2,3]

master_pred <- knn(train = master_training, test = master_test, cl = master_trainLabels$NetworkDomain, k = 3)

master_testLabels <- data.frame(master_testLabels)

merge <- data.frame(master_testLabels, master_pred)

pred <- table(merge)
(pred[1]+pred[4])/nrow(master_test)
# [1] 0.7419355

################################################################
#### use of caret package
library(caret)
library(dplyr)
master <- fread("removed_loops/output/removed_loops_measures.csv")

for(i in 1:length(master$ID)){
  if(master[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master[i, "NetworkDomain"] <- gsub(".*", "1", master[i, "NetworkDomain"])
  } else {
    master[i, "NetworkDomain"] <- gsub(".*", "0", master[i, "NetworkDomain"])
  }
}

master$NetworkDomain <- as.factor(master$NetworkDomain)
master <- na.omit(master)

## logistic regression
default_glm_mod <- train(
  form = NetworkDomain ~ AverageDegree + AveragePathLength +
    AverageTransitivity + BetweennessCentrality + Closeness + ClosenessCentrality + 
    DegreeAssortativity + DegreeCentrality + DegreeDistribution +
    Density + EigenvectorCentrality + EigenvectorCentrality_2 + 
    GlobalTransitivity,
  data = master,
  trControl = trainControl(method = "cv", number = 5),
  method = "glm",
  family = "binomial"
)
summary(default_glm_mod)

## train classifier with 100 and 500 iterations
# logistic regression; all variables
set.seed(1234)
index <- createDataPartition(master$NetworkDomain, times = 500, p=0.95, list=FALSE)

accuracy <- c()
for(i in 1:500){
  master_training <- master[index[, i],]
  master_test <- master[-index[, i],]
  default_glm_mod <- train(
    form = NetworkDomain ~ AverageDegree + AveragePathLength +
      AverageTransitivity + BetweennessCentrality + Closeness + ClosenessCentrality + 
      DegreeAssortativity + DegreeCentrality + DegreeDistribution +
      Density + EigenvectorCentrality + EigenvectorCentrality_2 + 
      GlobalTransitivity + Nodes + Edges,
    data = master,
    trControl = trainControl(method = "cv", number = 5),
    method = "glm",
    family = "binomial"
  )
  prediction <- predict(default_glm_mod, newdata = master_test)
  result <- table(master_test$NetworkDomain, prediction)
  accuracy <- combine(accuracy, (result[1] + result[4])/(22))
}
sd(accuracy)
mean(accuracy)
## 100 iterations
# SD : [1] 0.0757562
# mean : [1] 0.7959091

## 500 iterations
# SD : [1] 0.0865351
# mean : [1] 0.7940909








