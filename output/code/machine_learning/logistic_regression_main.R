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
master_data <- fread("output/master_measures.csv")

#### transform Social,Offline and Social,Online into factor variables with 2 levels
for(i in 1:nrow(master_data)){
  if(master_data[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master_data[i, "NetworkDomain"] <- gsub(".*", "1", master_data[i, "NetworkDomain"])
  } else {
    master_data[i, "NetworkDomain"] <- gsub(".*", "0", master_data[i, "NetworkDomain"])
  }
}

master_data$NetworkDomain <- as.factor(master_data$NetworkDomain)

# standardization
master_data[, 4:length(master_data)] <- data.table(apply(master_data[, 4:length(master_data)],
                                                         2, scale))


#### logistic regression model
logit_glm <- glm(NetworkDomain ~
                            Nodes + AveragePathLength + DegreeAssortativity +

                            AverageDegree +

                            GiniTransitivity +

                            GiniCloseness + GiniDegreeDistribution + GiniBetweenness,
                     family = binomial, data = master_data
                 )

summary(logit_glm)
plot(logit_glm)
accuracy <- c()
for(i in 1:5000){
  ind <- sample(2, nrow(master_data), replace=TRUE, prob=c(0.95, 0.05))

  master_test <- master_data[ind == 2, 3:24]

  pred <- predict(logit_glm2, newdata = master_test, type = "response")
  y_pred_num <- ifelse(pred > 0.5, 1, 0)
  y_pred <- factor(y_pred_num, levels=c(0, 1))
  y_act <- master_test$NetworkDomain
  accuracy[length(accuracy) + 1] <- mean(y_pred == y_act)
  rm(pred, y_pred, y_pred_num, y_act)
}


ind <- sample(2, nrow(master_data), replace=TRUE, prob=c(0.95, 0.05))

master_test <- master_data[ind == 2, 3:24]
master_training <- master_data[ind == 1, 3:24]

prediction <- predict(logit_glm2, newdata = master_test)
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
  form = NetworkDomain ~
    Nodes + Edges + AveragePathLength + DegreeAssortativity + Density +

    AverageTransitivity + GiniTransitivity + GlobalTransitivity +

    AverageDegree + MedianDegree +

    Complexity + Entropy +

    BetweennessCentrality + ClosenessCentrality + DegreeCentrality +
    EigenvectorCentrality +

    GiniBetweenness + GiniCloseness + GiniDegreeDistribution +
    GiniEigenvectorCentrality,
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
index <- createDataPartition(master_data$NetworkDomain, times = 1000, p=0.95, list=FALSE)

accuracy <- c()
a <- Sys.time()
for(i in 1:ncol(index)){
  master_training <- master_data[index[, i],]
  master_test <- master_data[-index[, i],]
  default_glm_mod <- train(
    form = NetworkDomain ~
      Nodes + AveragePathLength + DegreeAssortativity +

      AverageDegree +

      GiniTransitivity +

      GiniCloseness + GiniDegreeDistribution + GiniBetweenness,
    data = master_training,
    method = "glm",
    family = "binomial"
  )
  prediction <- predict(default_glm_mod, newdata = master_test)
  result <- table(master_test$NetworkDomain, prediction)
  accuracy[length(accuracy) + 1] <- (result[1]+result[4])/(nrow(master_test))
  b <- Sys.time()
}
b-a
sd(accuracy)
mean(accuracy)
summary(default_glm_mod)
plot(accuracy)
## 100 iterations
# SD : [1] 0.09098981
# mean : [1] 0.8095238

## 500 iterations
# SD : [1] 0.07644474
# mean : [1] 0.8103333


