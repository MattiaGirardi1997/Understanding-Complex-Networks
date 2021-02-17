# install packages
list.of.packages <- c("data.table", "dplyr", "mlbench", "caret", "e1071", "corrplot")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

#### logistic regression model ####
#### load in master
master_data <- fread("removed_loops/output/master_measures_removed_loops.csv")
master_data$NetworkDomain <- ifelse(master_data[, NetworkDomain] %in%
                                      c("Social,Offline", "Social,Online"), "Social", "Non-Social")

#### logistic regression model
scaled_glm_all <- train(NetworkDomain ~ GiniDegreeCount + GiniBetweenness + GiniTransitivity +
                            GiniEigenvectorCentrality + GiniCloseness +
                            MedianDegree + AverageDegree + AveragePathLength +
                            AverageTransitivity + BetweennessCentrality + ClosenessCentrality +
                            Complexity + DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
                            Density + EigenvectorCentrality + Entropy +
                            GlobalTransitivity + Nodes + Edges,
                        data = master_data,
                        trControl = trainControl(method = "cv", number = 5),
                        method = "glm", family = "binomial"
)

scaled_glm_removed_corr <- train(NetworkDomain ~ GiniDegreeCount + GiniCloseness +
                              AverageDegree + AveragePathLength +
                              AverageTransitivity + BetweennessCentrality + ClosenessCentrality +
                              DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
                              Density + Nodes + Edges,
                              data = master_data,
                              method = "glm", family = "binomial"
)

#network_glm <- train(NetworkDomain ~ GiniDegreeCount +
#                       GiniEigenvectorCentrality + GiniCloseness +
#                       MedianDegree + AveragePathLength + Complexity +
#                       BetweennessCentrality + ClosenessCentrality +
#                       Complexity + DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
#                       Density + GlobalTransitivity + Nodes + Edges, data = data.frame(NetworkDomain = master_data$NetworkDomain,
#                                                                                       apply(master_data[, 4:24], 2, scale))
#                     method = "glm",
#                     family = "binomial"
#)


#### use of caret package for machine learning ####
master_data <- fread("removed_loops/output/master_measures_removed_loops.csv")
master_data$NetworkDomain <- ifelse(master_data[, NetworkDomain] %in%
                                      c("Social,Offline", "Social,Online"), "Social", "Non-Social")

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
  method = "rf",
  family = "binomial"
)
summary(default_glm_mod)
summary(master_data)

## train classifier with 100 and 1000 iterations
# logistic regression; all variables
set.seed(1234)
index <- createDataPartition(master_data$NetworkDomain, times = 1000, p=0.95, list=FALSE)

accuracy <- c()
for(i in 1:ncol(index)){
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
# SD : [1] 0.08329545
# mean : [1] 0.7891667

## 1000 iterations
# SD : [1] 0.08230932
# mean : [1] 0.79575

# logistic regression; uncorrelated variables
set.seed(1234)
index <- createDataPartition(master_data$NetworkDomain, times = 1000, p=0.95, list=FALSE)

accuracy <- c()
for(i in 1:ncol(index)){
  master_training <- master_data[index[, i],]
  master_test <- master_data[-index[, i],]
  default_glm_mod <- train(
    form = NetworkDomain ~ GiniDegreeCount +
      GiniEigenvectorCentrality + GiniCloseness +
      MedianDegree + AveragePathLength + Complexity +
      BetweennessCentrality + ClosenessCentrality +
      Complexity + DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
      Density + GlobalTransitivity + Nodes,
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
# SD : [1] 0.09936668
# mean : [1] 0.735

## 500 iterations
# SD : [1] 0.08517322
# mean : [1] 0.7516667


