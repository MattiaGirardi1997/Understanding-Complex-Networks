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

#### load in master
master_data <- fread("removed_loops/output/master_measures_removed_loops.csv")

# ensure the results are repeatable
set.seed(1234)
# calculate correlation matrix
correlationMatrix <- cor(master_data[,4:24])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.75)
# print indexes of highly correlated attributes
names(master_data[,4:24])[print(highlyCorrelated)]

#### correlation matrix
cor <- cor(master_data[, 4:24], use = "complete.obs")

corrplot(cor, method = "color", type = "upper", tl.col = "black", tl.offset = 0.4,
         cl.align.text = "l", tl.srt = 90, addgrid.col = "black")


#### logistic regression model ####
#### load in master
master_data <- fread("removed_loops/output/master_measures_removed_loops.csv")

#### transform Social,Offline and Social,Online into logical variables
for(i in 1:length(master_data$ID)){
  if(master_data[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master_data[i, "NetworkDomain"] <- gsub(".*", "1", master_data[i, "NetworkDomain"])
  } else {
    master_data[i, "NetworkDomain"] <- gsub(".*", "0", master_data[i, "NetworkDomain"])
  }
}

master_data$NetworkDomain <- as.numeric(master_data$NetworkDomain)
master_data$NetworkDomain <- as.factor(master_data$NetworkDomain)
#### logistic regression model
scaled_network_glm <- glm(NetworkDomain ~ GiniDegreeCount +
                            GiniEigenvectorCentrality + GiniCloseness +
                            MedianDegree + AveragePathLength + Complexity +
                            BetweennessCentrality + ClosenessCentrality +
                            Complexity + DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
                            Density + GlobalTransitivity + Nodes + Edges,
                     family = binomial, data = data.frame(NetworkDomain = master_data$NetworkDomain,
                                                          apply(master_data[, 3:24], 2, scale))
                   )

network_glm <- train(NetworkDomain ~ GiniDegreeCount +
                     GiniEigenvectorCentrality + GiniCloseness +
                     MedianDegree + AveragePathLength + Complexity +
                     BetweennessCentrality + ClosenessCentrality +
                     Complexity + DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
                     Density + GlobalTransitivity + Nodes + Edges, data = data.frame(NetworkDomain = master_data$NetworkDomain,
                                                                             apply(master_data[, 4:24], 2, scale)),
                     trControl = trainControl(method = "cv", number = 5),
                     method = "glm",
                     family = "binomial"
)
summary(network_glm)
plot(network_glm)
accuracy <- c()
for(i in 1:5000){
  ind <- sample(2, nrow(master_data), replace=TRUE, prob=c(0.95, 0.05))

  master_test <- master_data[ind == 2, 3:24]

  pred <- predict(network_glm2, newdata = master_test, type = "response")
  y_pred_num <- ifelse(pred > 0.5, 1, 0)
  y_pred <- factor(y_pred_num, levels=c(0, 1))
  y_act <- master_test$NetworkDomain
  accuracy[length(accuracy) + 1] <- mean(y_pred == y_act)
  rm(pred, y_pred, y_pred_num, y_act)
}


ind <- sample(2, nrow(master_data), replace=TRUE, prob=c(0.95, 0.05))

master_test <- master_data[ind == 2, 3:24]
master_training <- master_data[ind == 1, 3:24]

prediction <- predict(network_glm2, newdata = master_test)
result <- table(master_test$NetworkDomain, prediction)
confusionMatrix(result)

mean(accuracy)
## Accuracy
# [1] 0.8251557

#### use of caret package for machine learning ####
master_data <- fread("removed_loops/output/master_measures_removed_loops.csv")
MedianDegree <- fread("removed_loops/output/median_degree.csv")[, 5]
GiniCoefficients <- fread("removed_loops/output/gini_coefficients.csv")[, 5:9]
Constraint <- fread("removed_loops/output/constraint.csv")[, 5:6]
master_data <- data.table(master_data, MedianDegree, GiniCoefficients, Constraint)
master_data <- na.omit(master_data)
rm(MedianDegree, GiniCoefficients, Constraint)

for(i in 1:length(master_data$ID)){
  if(master_data[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master_data[i, "NetworkDomain"] <- gsub(".*", "1", master_data[i, "NetworkDomain"])
  } else {
    master_data[i, "NetworkDomain"] <- gsub(".*", "0", master_data[i, "NetworkDomain"])
  }
}

master_data$NetworkDomain <- as.factor(master_data$NetworkDomain)
master_data <- na.omit(master_data)

## logistic regression
set.seed(1234)
default_glm_mod <- train(
  form = NetworkDomain ~ GiniDegreeCount + GiniBetweenness + GiniTransitivity +
    GiniEigenvectorCentrality + GiniCloseness +
    MedianDegree + AverageDegree + AveragePathLength +
    AverageTransitivity + BetweennessCentrality + ClosenessCentrality +
    Complexity + DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
    Density + EigenvectorCentrality + Entropy +
    GlobalTransitivity + Nodes + Edges,
  data = master_data,
  trControl = trainControl(method = "cv", number = 5),
  method = "glm",
  family = "binomial"
)
summary(default_glm_mod)
summary(master_data)
## train classifier with 100 and 500 iterations
# logistic regression; all variables
set.seed(1234)
index <- createDataPartition(master_data$NetworkDomain, times = 100, p=0.95, list=FALSE)

accuracy <- c()
for(i in 1:100){
  master_training <- master_data[index[, i],]
  master_test <- master_data[-index[, i],]
  default_glm_mod <- train(
    form = NetworkDomain ~ GiniDegreeCount + GiniBetweenness + GiniTransitivity +
      GiniEigenvectorCentrality + GiniCloseness +
      MedianDegree + AverageDegree + AveragePathLength +
      AverageTransitivity + BetweennessCentrality + ClosenessCentrality +
      Complexity + DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
      Density + EigenvectorCentrality + Entropy +
      GlobalTransitivity + Nodes + Edges,
    data = master_training,
    trControl = trainControl(method = "cv", number = 5),
    method = "glm",
    family = "binomial"
  )
  prediction <- predict(default_glm_mod, newdata = master_test)
  result <- table(master_test$NetworkDomain, prediction)
  accuracy[length(accuracy) + 1] <- (result[1]+result[4])/(24)
}
sd(accuracy)
mean(accuracy)

plot(accuracy)
## 100 iterations
# SD : [1] 0.09098981
# mean : [1] 0.8095238

## 500 iterations
# SD : [1] 0.07644474
# mean : [1] 0.8103333


##### Feature importance logistic regression #####
#### load in master
master_data <- fread("removed_loops/output/master_measures_removed_loops.csv")

#### transform Social,Offline and Social,Online into logical variables
for(i in 1:length(master_data$ID)){
  if(master_data[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master_data[i, "NetworkDomain"] <- gsub(".*", "1", master_data[i, "NetworkDomain"])
  } else {
    master_data[i, "NetworkDomain"] <- gsub(".*", "0", master_data[i, "NetworkDomain"])
  }
}

master_data$NetworkDomain <- as.factor(master_data$NetworkDomain)
master_data[, 3:24] <- apply(master_data[, 3:24], 2, scale)
apply(master_data[, 3:24], 2, scale)
#### logistic regression model
network_glm <- train(NetworkDomain ~ GiniDegreeCount +
                     GiniEigenvectorCentrality + GiniCloseness +
                     MedianDegree + AveragePathLength + Complexity +
                     BetweennessCentrality + ClosenessCentrality +
                     DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
                     Density + GlobalTransitivity + Nodes + Edges,
                   family = binomial, data = data.frame(NetworkDomain = master_data$NetworkDomain,
                                                        apply(master_data[, 3:24], 2, scale))
)
network_glm <- train(NetworkDomain ~ GiniDegreeCount +
                       GiniEigenvectorCentrality + GiniCloseness +
                       MedianDegree + AveragePathLength + Complexity +
                       BetweennessCentrality + ClosenessCentrality +
                       DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
                       Density + GlobalTransitivity + Nodes + Edges,
                          data = master_data,
                          method = "xgbLinear")

xgb_imp <- xgb.importance(feature_names = network_glm$finalModel$feature_names,
                          model = network_glm$finalModel)
xgb.ggplot.importance(xgb_imp)

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=5)

network_glm2 <- train(NetworkDomain ~ GiniDegreeCount +
                        GiniEigenvectorCentrality + GiniCloseness +
                        MedianDegree + AveragePathLength +
                        BetweennessCentrality + ClosenessCentrality +
                        DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
                        Density + GlobalTransitivity + Nodes + Edges,
                      data = master_data, method = "glm",
                      family = "binomial"
)
plot(varImp(network_glm2))


summary(network_glm)
summary(network_glm2)
vif(network_glm)
vif(network_glm2)

?train
