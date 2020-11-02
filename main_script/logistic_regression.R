##########################################
## Logistic Regression
## Mattia Girardi
## 02.11.2020
#########################################

# set working directory
setwd("~/Desktop/Bachelor Thesis/code/bachelor_thesis")

# install packages
list.of.packages <- c("data.table")
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
                    DegreeAssortativity + DegreeDistribution + Density + EigenvectorCentrality + GlobalTransitivity +
                    Reciprocity, family = binomial, data = master_measures)

summary(network_glm)

master_measures <- na.omit(master_measures)
glm.probs <- predict(network_glm,type = "response")

mean(glm.probs)

glm.pred <- ifelse(glm.probs > 0.5, "TRUE", "FALSE")

attach(master_measures)
table(glm.pred, NetworkDomain)


#### Boruta 
install.packages("Boruta")
library(Boruta)

master_measures <- na.omit(master_measures)


set.seed(100)
boruta.bank_train <- Boruta(NetworkDomain ~ AverageDegree + AveragePathLength + AverageTransitivity + Betweenness + Closeness +
                              DegreeAssortativity + DegreeDistribution + Density + EigenvectorCentrality + GlobalTransitivity +
                              Reciprocity, data = master_measures, doTrace = 3)
print(boruta.bank_train)


plot(boruta.bank_train, xlab = "", xaxt = "n")

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
                 DegreeAssortativity + DegreeDistribution + Density + EigenvectorCentrality + GlobalTransitivity +
                 Reciprocity, data=master_measures, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)


?trainControl()
?train
?varImp

normalize


#### train classifier
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

names(merge) <- c("Observed Domain", "Predicted Domain")

merge

### use of caret package
library(caret)
master_measures <- fread("output/master_measures.csv")

for(i in 1:length(master_measures$ID)){
  if(master_measures[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master_measures[i, "NetworkDomain"] <- gsub(".*", "1", master_measures[i, "NetworkDomain"])
  } else {
    master_measures[i, "NetworkDomain"] <- gsub(".*", "0", master_measures[i, "NetworkDomain"])
  }
}

master_measures$NetworkDomain <- as.factor(master_measures$NetworkDomain)
master_measures <- na.omit(master_measures)

index <- createDataPartition(master_measures$NetworkDomain, p=0.75, list=FALSE)

master_training <- master_measures[index,]
master_test <- master_measures[-index,]

model_knn <- train(master_training[, 4:14], master_training$NetworkDomain, method='logreg')

default_glm_mod <- train(
  form = NetworkDomain ~ AverageDegree + AveragePathLength + AverageTransitivity + Betweenness + Closeness +
    DegreeAssortativity + DegreeDistribution + Density + EigenvectorCentrality + GlobalTransitivity +
    Reciprocity,
  data = master_training[,3:14],
  trControl = trainControl(method = "cv", number = 5),
  method = "glm",
  family = "binomial"
)

summary(default_glm_mod)


prediction <- predict(default_glm_mod, newdata = master_test[, 3:14])

table(master_test$NetworkDomain, prediction)








