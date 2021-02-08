
#### load in master
master_data <- fread("removed_loops/output/master_measures_removed_loops.csv")

#### based on caret package
## transform Social,Offline and Social,Online into logical variables
for(i in 1:nrow(master_data)){
  if(master_data[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master_data[i, "NetworkDomain"] <- gsub(".*", "1", master_data[i, "NetworkDomain"])
  } else {
    master_data[i, "NetworkDomain"] <- gsub(".*", "0", master_data[i, "NetworkDomain"])
  }
}

master_data$NetworkDomain <- as.factor(master_data$NetworkDomain)

library(xgboost)
xgb_fit <- xgbtrain(NetworkDomain ~ GiniDegreeCount +
                   GiniEigenvectorCentrality + GiniCloseness +
                   MedianDegree + AveragePathLength + Complexity +
                   BetweennessCentrality + ClosenessCentrality +
                   Complexity + DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
                   Density + GlobalTransitivity + Nodes,
                 data = master_data,
                 method = "xgbTree")


caret_imp <- varImp(xgb_fit, scale = F)
importance_removed <- varImp(model_removed, scale=FALSE)
importance_removed2 <- varImp(model_removed2, scale=FALSE)
xgb_imp <- xgb.importance(feature_names = xgb_fit$finalModel$feature_names,
                          model = xgb_fit$finalModel)

glm <- varImp(glm_removed)
glm
plot(glm)

?xgb.attr()
plot(caret_imp)
xgb.plot.importance(xgb_imp) + theme_minimal()
plot(importance_removed)
plot(importance_removed2)
xgb.ggplot.importance(xgb_imp)
xgb.ggplot.importance(importance_removed)

### Caret
# Median Degree
# Gini Betweenness
# Nodes
# Gini Transitivity
# Gini Degree Count

### XG Boost
# Median Degree
# Gini Betweenness
# Nodes
# Gini Transitivity
# Gini Degree Count

### Regression linear vector quantization
# Gini Degree Distribution
# Median Degree
# Betweenness Centrality
# Entropy
# Mean Constraint

### Logistic Regression binomial
# Gini Degree Distribution
# Average Path Length
# Gini Betweenness
# Eigenvector Cebtrality
# Nodes










