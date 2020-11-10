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
index <- createDataPartition(master_measures$NetworkDomain, times = 100, p=0.95, list=FALSE)


i <- 1
accuracy <- c()
for(i in 1:100){
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

# AverageDegree 
[1] 0.5436364
# AveragePathLength 
[1] 0.5227273
# AverageTransitivity 
[1] 0.5568182

#± 12 errors
## glm.fit: fitted probabilities numerically 0 or 1 occurred
## Betweenness 
[1] 0.3636364

# Closeness 
[1] 0.5554545
# DegreeAssortativity 
[1] 0.7272727

## 11 errors
## glm.fit: fitted probabilities numerically 0 or 1 occurred
## DegreeDistribution 
[1] 0.7513636

## 29 errors
## glm.fit: fitted probabilities numerically 0 or 1 occurred
## Density 
[1] 0.7190909

# EigenvectorCentrality 
[1] 0.7195455

## 2 errors
## glm.fit: fitted probabilities numerically 0 or 1 occurred
## GlobalTransitivity 
[1] 0.7377273

#### all variables
## > 50 errors
[1] 0.7704545

################################################################################################
################################################################################################

master_measures_2 <- fread("output/undirected/master_measures_2.csv")
master_measures_2[master_measures_2 == 0 | master_measures_2 == 1] <- NA
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

set.seed(1234)
index <- createDataPartition(master_measures_2$NetworkDomain, times = 100, p=0.95, list=FALSE)

accuracy <- c()
for(i in 1:100){
  master_training <- master_measures_2[index[, i],]
  master_test <- master_measures_2[-index[, i],]
  default_glm_mod <- train(
    form = NetworkDomain ~ AverageDegree + AveragePathLength + AverageTransitivity + Closeness +
      DegreeAssortativity + DegreeDistribution + Density + EigenvectorCentrality + GlobalTransitivity,
    data = master_training,
    trControl = trainControl(method = "cv", number = 5),
    method = "glm",
    family = "binomial"
  )
  prediction <- predict(default_glm_mod, newdata = master_test)
  result <- table(master_test$NetworkDomain, prediction)
  accuracy <- combine(accuracy, (result[1] + result[4])/(27))
}

mean(accuracy)

# AverageDegree 
[1] 0.5918519
# AveragePathLength 
[1] 0.632963
# AverageTransitivity 
[1] 0.6307407
# Closeness 
[1] 0.6359259

## > 50 errors
## glm.fit: fitted probabilities numerically 0 or 1 occurred
# DegreeAssortativity 
[1] 0.7485185

## > 50 errors
## glm.fit: fitted probabilities numerically 0 or 1 occurred
## DegreeDistribution 
[1] 0.6818519

## > 50 errors
## glm.fit: fitted probabilities numerically 0 or 1 occurred
## Density 
[1] 0.6303704

# EigenvectorCentrality 
[1] 0.6425926

# GlobalTransitivity 
[1] 0.7251852

#### all variables
## > 50 errors
[1] 0.7685185