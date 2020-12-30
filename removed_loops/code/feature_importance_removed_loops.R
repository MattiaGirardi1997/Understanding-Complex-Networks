########################################################################################
#### caret package
# ensure results are repeatable
set.seed(7)
# load the library
# install packages
list.of.packages <- c("data.table", "igraph", "mlbench", "caret", "e1071")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)


master <- fread("removed_loops/output/removed_loops_measures.csv")
master <- na.omit(master)

# ensure the results are repeatable
set.seed(7)
# calculate correlation matrix
correlationMatrix <- cor(master[,4:18])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.75)
# print indexes of highly correlated attributes
names(master[,4:18])[print(highlyCorrelated)]


#### transform Social,Offline and Social,Online into logical variables
for(i in 1:length(master$ID)){
  if(master[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master[i, "NetworkDomain"] <- gsub(".*", "1", master[i, "NetworkDomain"])
  } else {
    master[i, "NetworkDomain"] <- gsub(".*", "0", master[i, "NetworkDomain"])
  }
}

master$NetworkDomain <- as.factor(master$NetworkDomain)
master <- na.omit(master)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=5)
# train the model
model <- train(NetworkDomain ~ AverageDegree + AveragePathLength +
                 AverageTransitivity + BetweennessCentrality + Closeness + ClosenessCentrality +
                 DegreeAssortativity + DegreeCentrality + DegreeDistribution +
                 Density + EigenvectorCentrality + EigenvectorCentrality_2 +
                 GlobalTransitivity + Nodes + Edges,
               data=master, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)


library(randomForest)
# ensure the results are repeatable
set.seed(7)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(master[,4:18], master[[3]], sizes=c(1:15), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))


