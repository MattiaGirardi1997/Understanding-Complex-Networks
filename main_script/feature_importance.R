########################################################################################
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


master_measures_2 <- fread("output/undirected/master_measures_2.csv")
master_measures_2 <- na.omit(master_measures_2)

# ensure the results are repeatable
set.seed(7)
# calculate correlation matrix
correlationMatrix <- cor(master_measures_2[,4:21])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.75)
# print indexes of highly correlated attributes
names(master_measures_2[,4:21])[print(highlyCorrelated)]


#### transform Social,Offline and Social,Online into logical variables
for(i in 1:length(master_measures_2$ID)){
  if(master_measures_2[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master_measures_2[i, "NetworkDomain"] <- gsub(".*", "1", master_measures_2[i, "NetworkDomain"])
  } else {
    master_measures_2[i, "NetworkDomain"] <- gsub(".*", "0", master_measures_2[i, "NetworkDomain"])
  }
}

master_measures_2$NetworkDomain <- as.factor(master_measures_2$NetworkDomain)
master_measures_2 <- na.omit(master_measures_2)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=5)
# train the model
model <- train(NetworkDomain ~ AverageComplexity + AverageDegree + AveragePathLength +
                 AverageTransitivity + BetweennessCentrality + Closeness + ClosenessCentrality +
                 Complexity + DegreeAssortativity + DegreeCentrality + DegreeDistribution +
                 Density + EigenvectorCentrality + EigenvectorCentrality_2 + Entropy +
                 GlobalTransitivity,
               data=master_measures_2, method="lvq", preProcess="scale", trControl=control)
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
results <- rfe(master_measures_2[,6:21], master_measures_2[[3]], sizes=c(1:15), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))


ggplot(master, aes(x = Edges, y = Mean, color = NetworkDomain)) + geom_point()
ggplot(master, aes(x = Nodes, y = Mean, color = NetworkDomain)) + geom_point()
ggplot(master, aes(x = AverageComplexity, y = Mean, color = NetworkDomain)) + geom_point()
ggplot(master, aes(x = AverageDegree, y = Mean, color = NetworkDomain)) + geom_point()
ggplot(master, aes(x = AveragePathLength, y = Mean, color = NetworkDomain)) + geom_point()
ggplot(master, aes(x = AverageTransitivity, y = Mean, color = NetworkDomain)) + geom_point()
ggplot(master, aes(x = BetweennessCentrality, y = Mean, color = NetworkDomain)) + geom_point()
ggplot(master, aes(x = Closeness, y = Mean, color = NetworkDomain)) + geom_point()
ggplot(master, aes(x = ClosenessCentrality, y = Mean, color = NetworkDomain)) + geom_point()
ggplot(master, aes(x = Complexity, y = Mean, color = NetworkDomain)) + geom_point()
ggplot(master, aes(x = DegreeAssortativity, y = Mean, color = NetworkDomain)) + geom_point()
ggplot(master, aes(x = DegreeCentrality, y = Mean, color = NetworkDomain)) + geom_point()
ggplot(master, aes(x = DegreeDistribution, y = Mean, color = NetworkDomain)) + geom_point()
ggplot(master, aes(x = Density, y = Mean, color = NetworkDomain)) + geom_point()
ggplot(master, aes(x = EigenvectorCentrality, y = Mean, color = NetworkDomain)) + geom_point()
ggplot(master, aes(x = EigenvectorCentrality_2, y = Mean, color = NetworkDomain)) + geom_point()
ggplot(master, aes(x = Entropy, y = Mean, color = NetworkDomain)) + geom_point()
ggplot(master, aes(x = GlobalTransitivity, y = Mean, color = NetworkDomain)) + geom_point()
