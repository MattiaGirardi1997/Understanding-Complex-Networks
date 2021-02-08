# install packages
list.of.packages <- c("data.table", "dplyr", "mlbench", "caret", "e1071", "corrplot")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

# load in dataset
master_measures_2 <- fread("removed_loops/output/master_measures_removed_loops.csv")
MedianDegree <- fread("removed_loops/output/median_degree.csv")[, 5]
GiniCoefficients <- fread("removed_loops/output/gini_coefficients.csv")[, 5:9]
Constraint <- fread("removed_loops/output/constraint.csv")[, 5:6]
master_measures_2 <- data.table(master_measures_2, MedianDegree, GiniCoefficients, Constraint)
master_measures_2 <- na.omit(master_measures_2)

# transform Social,Offline and Social,Online into logical variables
for(i in 1:length(master_measures_2$ID)){
  if(master_measures_2[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master_measures_2[i, "NetworkDomain"] <- gsub(".*", "1", master_measures_2[i, "NetworkDomain"])
  } else {
    master_measures_2[i, "NetworkDomain"] <- gsub(".*", "0", master_measures_2[i, "NetworkDomain"])
  }
}

set.seed(1234)
accuracy <- c()
prob <- c(0.8, 0.85, 0.9, 0.95, 0.99)
p <- prob[1]
k <- 1
i <- 1
for(j in 1:length(prob)){
  p <- prob[j]
  for(k in 1:15){
    accuracy <- c()
    for(i in 1:1000){
      #determine data partition
      ind <- sample(2, nrow(master_measures_2), replace=TRUE, prob=c(p, 1-p))
      table(ind)
      #determine training and test data
      training_data <- master_measures_2[ind == 1, 6:28]
      test_data <- master_measures_2[ind == 2, 6:28]

      # determine trai
      trainLabels <- master_measures_2[ind==1,3]
      testLabels <- master_measures_2[ind==2,3]

      # predict test data labels with K-Nearest Neighbor (KNN) method
      pred <- knn(train = training_data, test = test_data,
                         cl = trainLabels$NetworkDomain, k = k)

      # convert testLabels to a data frame
      testLabels <- data.frame(testLabels)

      # merge testLabels and predictions
      merge <- data.frame(testLabels, pred)

      # create confusion matrix
      pred <- table(merge)

      # compute accuracy
      accuracy[length(accuracy) + 1] <- (pred[1]+pred[4])/(nrow(merge))
    }
    if(j == 1 & k == 1){
      write.table(data.table(K_Neighbors = k, TrainingData = sprintf("%s%%", p*100),
                             Accuracy = mean(na.omit(accuracy))),
                  file = "removed_loops/output/knn_machine_learning.csv", row.names = F, sep = ",")
    } else {
      write.table(data.table(k, sprintf("%s%%", p*100), mean(na.omit(accuracy))),
                  file = "removed_loops/output/knn_machine_learning.csv", row.names = F, sep = ",",
                  col.names = F, append = T)
    }
  }
}

knn <- fread("removed_loops/output/knn_machine_learning.csv")

ggplot(knn[!is.na(knn$Accuracy),], aes(y = Accuracy, x = K_Neighbors, color = TrainingData)) + geom_line()

mean(na.omit(knn$Accuracy))
