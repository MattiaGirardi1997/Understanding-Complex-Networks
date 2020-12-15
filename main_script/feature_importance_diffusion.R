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

results <- fread("output/diffusion/consolidated/results_1.csv")[, Mean]
master_measures_2 <- data.table(fread("output/undirected/master_measures_2.csv"), Mean = results)
master_measures_2 <- na.omit(master_measures_2)

# ensure the results are repeatable
set.seed(7)
# calculate correlation matrix
correlationMatrix <- cor(master_measures_2[,4:21])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.5)
# print indexes of highly correlated attributes
names(master_measures_2[,4:21])[print(highlyCorrelated)]


# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=5)
# train the model
model <- train(Mean ~ AverageComplexity + AverageDegree + AveragePathLength +
                 AverageTransitivity + BetweennessCentrality + Closeness + ClosenessCentrality +
                 Complexity + DegreeAssortativity + DegreeCentrality + DegreeDistribution +
                 Density + EigenvectorCentrality + EigenvectorCentrality_2 + Entropy +
                 GlobalTransitivity,
               data=master_measures_2, method="glmboost")
# BstLm
# lm
# glm

summary(model)
# estimate variable importance
importance <- varImp(model, scale=F)
# summarize importance
print(importance)
# plot importance
plot(importance)


library(randomForest)
# ensure the results are repeatable
set.seed(7)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="boot", number=10)
# run the RFE algorithm
results <- rfe(master_measures_2[,6:21], master_measures_2[[22]], sizes=c(1:15), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

?rfeControl




model <- lm(Mean ~ AverageComplexity + AverageDegree + AveragePathLength +
              AverageTransitivity + BetweennessCentrality + Closeness + ClosenessCentrality +
              Complexity + DegreeAssortativity + DegreeCentrality + DegreeDistribution +
              Density + EigenvectorCentrality + EigenvectorCentrality_2 + Entropy +
              GlobalTransitivity + Nodes + Edges,
            data=master_measures_2)


summary(model)
car::vif(model)






