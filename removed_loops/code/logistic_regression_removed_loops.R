##########################################
## Logistic Regression
## Mattia Girardi
## 02.11.2020
#########################################

# set working directory
setwd("~/Desktop/Bachelor Thesis/code/bachelor_thesis")

# install packages
list.of.packages <- c("data.table", "dplyr", "mlbench", "caret", "e1071", "corrplot")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

#### analyze correlation ####
master_measures_2 <- fread("removed_loops/output/master_measures_removed_loops.csv")
MedianDegree <- fread("removed_loops/output/median_degree.csv")[, 5]
GiniCoefficients <- fread("removed_loops/output/gini_coefficients.csv")[, 5:9]
Constraint <- fread("removed_loops/output/constraint.csv")[, 5:6]
master_measures_2 <- data.table(master_measures_2, MedianDegree, GiniCoefficients, Constraint)
master_measures_2 <- na.omit(master_measures_2)

# ensure the results are repeatable
set.seed(1234)
# calculate correlation matrix
correlationMatrix <- cor(master_measures_2[,4:28])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.75)
# print indexes of highly correlated attributes
names(master_measures_2[,4:28])[print(highlyCorrelated)]

#### correlation matrix
cor <- cor(master_measures_2[, 4:28], use = "complete.obs")

corrplot(cor, method = "color", type = "upper", tl.col = "black", tl.offset = 0.4,
         cl.align.text = "l", tl.srt = 90, addgrid.col = "black")


#### logistic regression model ####
#### load in master
master_measures_2 <- fread("removed_loops/output/master_measures_removed_loops.csv")
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
network_glm_2 <- glm(NetworkDomain ~ GiniDegreeCount + GiniBetweenness + GiniTransitivity +
                       GiniEigenvectorCentrality + GiniCloseness + MeanConstraint + GiniConstraint +
                       MedianDegree + AverageDegree + AveragePathLength +
                       AverageTransitivity + BetweennessCentrality + Closeness + ClosenessCentrality +
                       Complexity + DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
                       Density + EigenvectorCentrality + EigenvectorCentrality_2 + Entropy +
                       GlobalTransitivity + Nodes + Edges,
                     family = binomial, data = master_measures_2)

summary(network_glm_2)

pred <- predict(network_glm_2, newdata = master_test, type = "response")
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- master_test$NetworkDomain
mean(y_pred == y_act)

# Accuracy [1] 0.9166667

#### train classifier ####
library(class)
# load in master
master_measures_2 <- fread("removed_loops/output/master_measures_removed_loops.csv")

# transform Social,Offline and Social,Online into logical variables
for(i in 1:length(master_measures_2$ID)){
  if(master_measures_2[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master_measures_2[i, "NetworkDomain"] <- gsub(".*", "1", master_measures_2[i, "NetworkDomain"])
  } else {
    master_measures_2[i, "NetworkDomain"] <- gsub(".*", "0", master_measures_2[i, "NetworkDomain"])
  }
}

master_measures_2$NetworkDomain <- as.factor(master_measures_2$NetworkDomain)

master_measures_2 <- na.omit(master_measures_2)

set.seed(1234)
accuracy <- c()
for(i in 1:1000){
  ind <- sample(2, nrow(master_measures_2), replace=TRUE, prob=c(0.95, 0.33))

  master_training <- master_measures_2[ind == 1, 6:28]
  master_test <- master_measures_2[ind == 2, 6:28]

  master_trainLabels <- master_measures_2[ind==1,3]
  master_testLabels <- master_measures_2[ind==2,3]

  master_pred <- knn(train = master_training, test = master_test, cl = master_trainLabels$NetworkDomain, k = 3)

  master_testLabels <- data.frame(master_testLabels)

  merge <- data.frame(master_testLabels, master_pred)

  pred <- table(merge)
  accuracy[length(accuracy) + 1] <- (pred[1]+pred[4])/(nrow(merge))
}

mean(accuracy)
# [1] 0.7944481

#### use of caret package for machine learning ####
master_measures_2 <- fread("removed_loops/output/master_measures_removed_loops.csv")
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
set.seed(1234)
default_glm_mod <- train(
  form = NetworkDomain ~ GiniTransitivity +
    GiniEigenvectorCentrality + MeanConstraint + GiniConstraint +
    MedianDegree + AverageDegree + AveragePathLength +
    AverageTransitivity + BetweennessCentrality + Closeness + ClosenessCentrality +
    Complexity + DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
    Density + EigenvectorCentrality + EigenvectorCentrality_2 + Entropy +
    GlobalTransitivity + Nodes + Edges,
  data = master_measures_2,
  trControl = trainControl(method = "cv", number = 50),
  method = "glm",
  family = "binomial"
)
summary(default_glm_mod)

## train classifier with 100 and 500 iterations
# logistic regression; all variables; only undirected networks
set.seed(1234)
index <- createDataPartition(master_measures_2$NetworkDomain, times = 5, p=0.95, list=FALSE)

accuracy <- c()
for(i in 1:5){
  master_training <- master_measures_2[index[, i],]
  master_test <- master_measures_2[-index[, i],]
  default_glm_mod <- train(
    form = NetworkDomain ~ GiniTransitivity +
      GiniEigenvectorCentrality + MeanConstraint + GiniConstraint +
      MedianDegree + AverageDegree + AveragePathLength +
      AverageTransitivity + BetweennessCentrality + Closeness + ClosenessCentrality +
      Complexity + DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
      Density + EigenvectorCentrality + EigenvectorCentrality_2 + Entropy +
      GlobalTransitivity + Nodes + Edges,
    data = master_training,
    trControl = trainControl(method = "cv", number = 5),
    method = "glm",
    family = "binomial"
  )
  prediction <- predict(default_glm_mod, newdata = master_test)
  result <- table(master_test$NetworkDomain, prediction)
  accuracy <- combine(accuracy, (result[1] + result[4])/(24))
}
sd(accuracy)
mean(accuracy)

## 100 iterations
# SD : [1] 0.09098981
# mean : [1] 0.76375

## 500 iterations
# SD : [1] 0.08930955
# mean : [1] 0.7470833

# logistic regression; significant variables; only undirected networks
set.seed(1234)
index <- createDataPartition(master_measures_2$NetworkDomain, times = 500, p=0.95, list=FALSE)

accuracy <- c()
for(i in 1:500){
  master_training <- master_measures_2[index[, i],]
  master_test <- master_measures_2[-index[, i],]
  default_glm_mod <- train(
    form = NetworkDomain ~ AverageDegree + BetweennessCentrality +
      DegreeAssortativity + ClosenessCentrality + DegreeAssortativity +
      EigenvectorCentrality + EigenvectorCentrality_2 + Entropy + Edges,
    data = master_training,
    trControl = trainControl(method = "cv", number = 5),
    method = "glm",
    family = "binomial"
  )
  prediction <- predict(default_glm_mod, newdata = master_test)
  result <- table(master_test$NetworkDomain, prediction)
  accuracy <- combine(accuracy, (result[1] + result[4])/24)
}
sd(accuracy)
mean(accuracy)

## 100 iterations
# SD : [1] 0.08791588
# mean : [1] 0.7604167

## 500 iterations
# SD : [1] 0.08545789
# mean : [1] 0.7576667




set.seed(1234)
index <- createDataPartition(master_measures_2$NetworkDomain, times = 50, p=0.95, list=FALSE)

accuracy <- c()
for(i in 1:50){
  master_training <- master_measures_2[index[, i],]
  master_test <- master_measures_2[-index[, i],]
  default_glm_mod <- train(
    form = NetworkDomain ~ GiniDegreeDistribution + MedianDegree +
      BetweennessCentrality + Entropy + MeanConstraint,
    data = master_training,
    trControl = trainControl(method = "cv", number = 5),
    method = "glm",
    family = "binomial"
  )
  prediction <- predict(default_glm_mod, newdata = master_test)
  result <- table(master_test$NetworkDomain, prediction)
  accuracy <- combine(accuracy, (result[1] + result[4])/(24))
}
sd(accuracy)
mean(accuracy)

for(i in 1:50){
  master_training <- master_measures_2[index[, i],]
  master_test <- master_measures_2[-index[, i],]
  default_glm_mod <- train(
    form = NetworkDomain ~ GiniDegreeDistribution + AveragePathLength + GiniBetweenness +
      EigenvectorCentrality + Nodes,
    data = master_training,
    trControl = trainControl(method = "cv", number = 5),
    method = "glm",
    family = "binomial"
  )
  prediction <- predict(default_glm_mod, newdata = master_test)
  result <- table(master_test$NetworkDomain, prediction)
  accuracy <- combine(accuracy, (result[1] + result[4])/(24))
}

