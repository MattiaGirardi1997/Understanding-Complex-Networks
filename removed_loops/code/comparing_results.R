########################
######## install packages ########
list.of.packages <- c("data.table", "igraph", "jsonlite", "ggplot2", "gridExtra", "mlbench",
                      "caret", "e1071", "corrplot")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

#### pie chart of domains
prct <- prop.table(table(removed$NetworkDomain))
colors <- c("Light Green", "Light Blue", "Khaki", "tomato2", "tomato3", "pink", "plum2")
pie(prct, col = colors, radius = 1.5)





########################
######## comparing number of edges and nodes before and after removing loops ########
removed <- fread("removed_loops/output/master_measures_removed_loops.csv")
MedianDegree <- fread("removed_loops/output/median_degree.csv")[, 5]
GiniCoefficients <- fread("removed_loops/output/gini_coefficients.csv")[, 5:9]
Constraint <- fread("removed_loops/output/constraint.csv")[, 5:6]
removed <- data.table(removed, MedianDegree, GiniCoefficients, Constraint)
removed <- na.omit(removed)

master_data <- fread("output/undirected/master_measures_2.csv")
master_data <- master_data[master_data$Name %in% removed$Name]

table(removed$NetworkDomain)

## split domains into Social and Non-Social
for(i in 1:nrow(removed)){
  if(removed[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    removed[i, "NetworkDomain"] <- gsub(".*", "Social", removed[i, "NetworkDomain"])
  } else {
    removed[i, "NetworkDomain"] <- gsub(".*", "Non-Social", removed[i, "NetworkDomain"])
  }
}

for(i in 1:nrow(master_data)){
  if(master_data[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master_data[i, "NetworkDomain"] <- gsub(".*", "Social", master_data[i, "NetworkDomain"])
  } else {
    master_data[i, "NetworkDomain"] <- gsub(".*", "Non-Social", master_data[i, "NetworkDomain"])
  }
}

ggplot() +
  geom_line(data = removed, aes(x = Nodes, y = Edges), color = "black") +
  geom_line(data = master_data, aes(x = Nodes, y = Edges), color = "red")


######## comparing measure results ########
removed <- fread("removed_loops/output/master_measures_removed_loops.csv")
MedianDegree <- fread("removed_loops/output/median_degree.csv")[, 5]
GiniCoefficients <- fread("removed_loops/output/gini_coefficients.csv")[, 5:9]
Constraint <- fread("removed_loops/output/constraint.csv")[, 5:6]
removed <- data.table(removed, MedianDegree, GiniCoefficients, Constraint)
removed <- na.omit(removed)
master_data <- fread("output/undirected/master_measures_2.csv")
master_data <- master_data[master_data$Name %in% removed$Name]

for(i in 1:nrow(removed)){
  if(removed[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    removed[i, "NetworkDomain"] <- gsub(".*", "Social", removed[i, "NetworkDomain"])
  } else {
    removed[i, "NetworkDomain"] <- gsub(".*", "Non-Social", removed[i, "NetworkDomain"])
  }
}

for(i in 1:nrow(removed)){
  if(removed[i, NetworkDomain] == "Social,Offline"){
    removed[i, "NetworkDomain"] <- gsub(".*", "Offline", removed[i, "NetworkDomain"])
  } else if (removed[i, NetworkDomain] == "Social,Online") {
    removed[i, "NetworkDomain"] <- gsub(".*", "Online", removed[i, "NetworkDomain"])
  } else {
    removed[i, "NetworkDomain"] <- gsub(".*", "Non-Social", removed[i, "NetworkDomain"])
  }
}

for(i in 1:nrow(master_data)){
  if(master_data[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master_data[i, "NetworkDomain"] <- gsub(".*", "Social", master_data[i, "NetworkDomain"])
  } else {
    master_data[i, "NetworkDomain"] <- gsub(".*", "Non-Social", master_data[i, "NetworkDomain"])
  }
}

### Median
# based on domain
ggplot(removed, aes(x = Nodes, y = Median_degree, color = NetworkDomain)) +
  geom_point() + scale_x_log10() + scale_y_log10()

### Gini degree
# based on domain
ggplot(removed, aes(x = Nodes, y = gini_d.Gini, color = NetworkDomain)) +
  geom_point() + scale_x_log10() + scale_y_log10()

### Gini Transitivity
# based on domain
ggplot(removed, aes(x = Nodes, y = gini_t.Gini, color = NetworkDomain)) +
  geom_point() + scale_x_log10() + scale_y_log10()

### Gini Constraint
# based on domain
ggplot(removed, aes(x = Nodes, y = constraint_gini, color = NetworkDomain)) +
  geom_point() + scale_x_log10() + scale_y_log10()

### Mean Constraiint
# based on domain
ggplot(removed, aes(x = Nodes, y = constraint_mean, color = NetworkDomain)) +
  geom_point() + scale_x_log10() + scale_y_log10()


### Average Degree
ggplot() +
  geom_point(data=removed,aes(x=Nodes, y=AverageDegree), color='green') +
  geom_point(data=master_data, aes(x=Nodes, y=AverageDegree), color='red') +
  scale_x_log10() + scale_y_log10()

# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = AverageDegree, color = NetworkDomain)) +
               geom_point() + scale_x_log10() + scale_y_log10(), ggplot(master_data, aes(x = Nodes, y = AverageDegree,
                                                                                         color = NetworkDomain)) + geom_point()
             + scale_x_log10() + scale_y_log10())

### Average Path Length
ggplot() +
  geom_point(data=removed,aes(x=Nodes, y=AveragePathLength), color='green') +
  geom_point(data=master_data, aes(x=Nodes, y=AveragePathLength), color='red') +
  scale_x_log10() + scale_y_log10()
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = AveragePathLength, color = NetworkDomain)) +
               geom_point() + scale_x_log10() + scale_y_log10(), ggplot(master_data, aes(x = Nodes, y = AveragePathLength,
                                                                                         color = NetworkDomain)) + geom_point()
             + scale_x_log10() + scale_y_log10())

### Average Transitivity
ggplot() +
  geom_point(data=removed,aes(x=Nodes, y=AverageTransitivity), color='green') +
  geom_point(data=master_data, aes(x=Nodes, y=AverageTransitivity), color='red') +
  scale_x_log10() + scale_y_log10()
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = AverageTransitivity, color = NetworkDomain)) +
               geom_point() + scale_x_log10() + scale_y_log10(), ggplot(master_data, aes(x = Nodes, y = AverageTransitivity,
                                                                                         color = NetworkDomain)) + geom_point()
             + scale_x_log10() + scale_y_log10())

### Betweenness Centrality
ggplot() +
  geom_point(data=removed,aes(x=Nodes, y=BetweennessCentrality), color='green') +
  geom_point(data=master_data, aes(x=Nodes, y=BetweennessCentrality), color='red') +
  scale_x_log10() + scale_y_log10()
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = BetweennessCentrality, color = NetworkDomain)) +
               geom_point() + scale_x_log10() + scale_y_log10(), ggplot(master_data, aes(x = Nodes, y = BetweennessCentrality,
                                                                                         color = NetworkDomain)) + geom_point()
             + scale_x_log10() + scale_y_log10())

### Complexity
ggplot() +
  geom_point(data=removed,aes(x=Nodes, y=Complexity), color='green') +
  geom_point(data=master_data, aes(x=Nodes, y=Complexity), color='red')
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = Complexity, color = NetworkDomain)) +
               geom_point() + scale_x_log10() + scale_y_log10(), ggplot(master_data, aes(x = Nodes, y = Complexity,
                                                                                         color = NetworkDomain)) + geom_point() +
               scale_x_log10())

### Closeness
ggplot() +
  geom_point(data=removed,aes(x=Nodes, y=Closeness), color='green') +
  geom_point(data=master_data, aes(x=Nodes, y=Closeness), color='red')
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = Closeness, color = NetworkDomain)) +
               geom_point() + scale_x_log10() + scale_y_log10(), ggplot(master_data, aes(x = Nodes, y = Closeness,
                                                                                         color = NetworkDomain)) + geom_point()
             + scale_x_log10() + scale_y_log10())

### Closeness Centrality
ggplot() +
  geom_point(data=removed,aes(x=Nodes, y=ClosenessCentrality), color='green') +
  geom_point(data=master_data, aes(x=Nodes, y=ClosenessCentrality), color='red')
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = ClosenessCentrality, color = NetworkDomain)) +
               geom_point() + scale_x_log10() + scale_y_log10(), ggplot(master_data, aes(x = Nodes, y = ClosenessCentrality,
                                                                                         color = NetworkDomain)) + geom_point()
             + scale_x_log10() + scale_y_log10())

### Degree Assortativity
ggplot() +
  geom_point(data=removed,aes(x=Nodes, y=DegreeAssortativity), color='green') +
  geom_point(data=master_data, aes(x=Nodes, y=DegreeAssortativity), color='red')
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = DegreeAssortativity, color = NetworkDomain)) +
               geom_point() + scale_x_log10(), ggplot(master_data, aes(x = Nodes, y = DegreeAssortativity,
                                                                       color = NetworkDomain)) + geom_point()
             + scale_x_log10())

### Degree Centrality
ggplot() +
  geom_point(data=removed,aes(x=Nodes, y=DegreeCentrality), color='green') +
  geom_point(data=master_data, aes(x=Nodes, y=DegreeCentrality), color='red')
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = DegreeCentrality, color = NetworkDomain)) +
               geom_point() + scale_x_log10() + scale_y_log10(), ggplot(master_data, aes(x = Nodes, y = DegreeCentrality,
                                                                                         color = NetworkDomain)) + geom_point() +
               scale_x_log10() + scale_y_log10())

### Degree Distribution
ggplot() +
  geom_point(data=removed,aes(x=Nodes, y=DegreeDistribution), color='green') +
  geom_point(data=master_data, aes(x=Nodes, y=DegreeDistribution), color='red')
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = DegreeDistribution, color = NetworkDomain)) +
               geom_point() + scale_x_log10() + scale_y_log10(), ggplot(master_data, aes(x = Nodes, y = DegreeDistribution,
                                                                                         color = NetworkDomain)) + geom_point() +
               scale_x_log10() + scale_y_log10())

### Density
ggplot() +
  geom_point(data=removed,aes(x=Nodes, y=Density), color='green') +
  geom_point(data=master_data, aes(x=Nodes, y=Density), color='red')
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = Density, color = NetworkDomain)) +
               geom_point() + scale_x_log10(), ggplot(master_data, aes(x = Nodes, y = Density,
                                                                       color = NetworkDomain)) + geom_point())

### Eigenvector Centrality
ggplot() +
  geom_point(data=removed,aes(x=Nodes, y=EigenvectorCentrality), color='green') +
  geom_point(data=master_data, aes(x=Nodes, y=EigenvectorCentrality), color='red')
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = EigenvectorCentrality, color = NetworkDomain)) +
               geom_point() + scale_x_log10() + scale_y_log10(), ggplot(master_data, aes(x = Nodes, y = EigenvectorCentrality,
                                                                                         color = NetworkDomain)) + geom_point())

### Eigenvector Centrality 2
ggplot() +
  geom_point(data=removed,aes(x=Nodes, y=EigenvectorCentrality_2), color='green') +
  geom_point(data=master_data, aes(x=Nodes, y=EigenvectorCentrality_2), color='red')
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = EigenvectorCentrality_2, color = NetworkDomain)) +
               geom_point() + scale_x_log10() + scale_y_log10(), ggplot(master_data, aes(x = Nodes, y = EigenvectorCentrality_2,
                                                                                         color = NetworkDomain)) + geom_point())

### Entropy
ggplot() +
  geom_point(data=removed,aes(x=Nodes, y=Entropy), color='green') +
  geom_point(data=master_data, aes(x=Nodes, y=Entropy), color='red')
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = Entropy, color = NetworkDomain)) +
               geom_point() + scale_x_log10() + scale_y_log10(), ggplot(master_data, aes(x = Nodes, y = Entropy,
                                                                                         color = NetworkDomain)) + geom_point())

### Global Transivity
ggplot() +
  geom_point(data=removed,aes(x=Nodes, y=GlobalTransitivity), color='green') +
  geom_point(data=master_data, aes(x=Nodes, y=GlobalTransitivity), color='red')
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = GlobalTransitivity, color = NetworkDomain)) +
               geom_point() + scale_x_log10() + scale_y_log10(), ggplot(master_data, aes(x = Nodes, y = GlobalTransitivity,
                                                                                         color = NetworkDomain)) + geom_point())


######## comparing feature importance ########
removed <- fread("removed_loops/output/master_measures_removed_loops.csv")
MedianDegree <- fread("removed_loops/output/median_degree.csv")[, 5]
GiniCoefficients <- fread("removed_loops/output/gini_coefficients.csv")[, 5:9]
Constraint <- fread("removed_loops/output/constraint.csv")[, 5:6]
removed <- data.table(removed, MedianDegree, GiniCoefficients, Constraint)
removed <- na.omit(removed)
master_data <- fread("output/undirected/master_measures_2.csv")
master_data <- master_data[master_data$Name %in% removed$Name]

#### based on caret package
## transform Social,Offline and Social,Online into logical variables
for(i in 1:nrow(removed)){
  if(removed[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    removed[i, "NetworkDomain"] <- gsub(".*", "1", removed[i, "NetworkDomain"])
  } else {
    removed[i, "NetworkDomain"] <- gsub(".*", "0", removed[i, "NetworkDomain"])
  }
}

for(i in 1:nrow(master_data)){
  if(master_data[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master_data[i, "NetworkDomain"] <- gsub(".*", "1", master_data[i, "NetworkDomain"])
  } else {
    master_data[i, "NetworkDomain"] <- gsub(".*", "0", master_data[i, "NetworkDomain"])
  }
}

removed$NetworkDomain <- as.factor(removed$NetworkDomain)

master_data$NetworkDomain <- as.factor(master_data$NetworkDomain)
master_data <- na.omit(master_data)
removed <- removed[removed$Name %in% master_data$Name]

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=5)
# train the model
model_removed <- train(NetworkDomain ~ GiniDegreeCount + GiniBetweenness + GiniTransitivity +
                         GiniEigenvectorCentrality + GiniCloseness + MeanConstraint + GiniConstraint +
                         MedianDegree + AverageDegree + AveragePathLength +
                         AverageTransitivity + BetweennessCentrality + Closeness + ClosenessCentrality +
                         Complexity + DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
                         Density + EigenvectorCentrality + EigenvectorCentrality_2 + Entropy +
                         GlobalTransitivity + Nodes + Edges,
                       data=removed, method="lvq", preProcess="scale", trControl=control)

model <- train(NetworkDomain ~ AverageDegree + AveragePathLength +
                 AverageTransitivity + BetweennessCentrality + Closeness + ClosenessCentrality +
                 Complexity + DegreeAssortativity + DegreeCentrality + DegreeDistribution +
                 Density + EigenvectorCentrality + EigenvectorCentrality_2 + Entropy +
                 GlobalTransitivity + Nodes + Edges,
               data=master_data, method="lvq", preProcess="scale", trControl=control)

model_removed <- train(
  form = NetworkDomain ~ GiniDegreeCount + GiniBetweenness + GiniTransitivity +
    GiniEigenvectorCentrality + GiniCloseness + MeanConstraint + GiniConstraint +
    MedianDegree + AverageDegree + AveragePathLength +
    AverageTransitivity + BetweennessCentrality + Closeness + ClosenessCentrality +
    Complexity + DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
    Density + EigenvectorCentrality + EigenvectorCentrality_2 + Entropy +
    GlobalTransitivity + Nodes + Edges,
  data = master_measures_2,
  trControl = trainControl(method = "cv", number = 5),
  method = "glm",
  family = "binomial"
)
# estimate variable importance
importance_removed <- varImp(model_removed, scale=FALSE)
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance_removed)
print(importance)
# plot importance
a <- plot(importance_removed)
b <- plot(importance)
grid.arrange(a)


#### based on Cross-Validation
library(randomForest)
# ensure the results are repeatable
set.seed(1234)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results_removed <- rfe(removed[,4:23], removed[[3]], sizes=c(1:5), rfeControl=control)
results <- rfe(master_data[,4:20], master_data[[3]], sizes=c(1:18), rfeControl=control)
?rfe
# summarize the results
print(results_removed)
print(results)
# list the chosen features
predictors(results_removed)[1:5]
predictors(results)[1:5]
# plot the results
a <- plot(results_removed, type=c("g", "o"))
b <- plot(results, type=c("g", "o"))
grid.arrange(a, b)


# compute feautre importance with several steps
for(i in 1:5){
  control <- rfeControl(functions=rfFuncs, method="cv", number=10)
  results_removed <- rfe(removed[,4:28], removed[[3]], sizes=c(1:4), rfeControl=control)
  imp <- predictors(results_removed)
  if(i == 1){
    write.table(data.table(Rank1 = imp[1], Rank2 = imp[2], Rank3 = imp[3], Rank4 = imp[4],
                           Rank5 = imp[5]), file = "removed_loops/output/feature_rank2.csv", sep = ",",
                row.names = F)
  } else {
    write.table(data.table(imp[1], imp[2], imp[3], imp[4], imp[5]), file = "removed_loops/output/feature_rank2.csv",
                row.names = F, sep = ",", col.names = F, append = T)
  }
}

ranked <- fread("removed_loops/output/feature_rank2.csv")
ranked$ID <- 1:nrow(ranked)
master <- melt(ranked, id.vars = "ID", variable.name = "category",
               value.name="scores")

ggplot(master, aes(x = category, fill = scores)) + geom_bar(position = "fill")


######## comparing diffusion results ########

### comparing removed_loops diffusion to standard diffusion
n <- fread("removed_loops/diffusion/10% starting_100% prob_70% threshold_1.csv")
n2 <- fread("removed_loops/diffusion/10% starting_100% prob_70% threshold_2.csv")
n3 <- fread("removed_loops/diffusion/10% starting_100% prob_70% threshold_3.csv")
n4 <- fread("removed_loops/diffusion/10% starting_100% prob_70% threshold_4.csv")
n5 <- fread("removed_loops/diffusion/10% starting_100% prob_70% threshold_5.csv")
n6 <- fread("removed_loops/diffusion/10% starting_100% prob_70% threshold_6.csv")
n7 <- fread("removed_loops/diffusion/10% starting_100% prob_70% threshold_7.csv")
n8 <- fread("removed_loops/diffusion/10% starting_100% prob_70% threshold_8.csv")
n$Iterations_1 <- as.integer(n$Iterations_1)
n2$Iterations_1 <- as.integer(n2$Iterations_1)
n3$Iterations_1 <- as.integer(n3$Iterations_1)
n4$Iterations_1 <- as.integer(n4$Iterations_1)
n5$Iterations_1 <- as.integer(n5$Iterations_1)
n6$Iterations_1 <- as.integer(n6$Iterations_1)
n7$Iterations_1 <- as.integer(n7$Iterations_1)
n8$Iterations_1 <- as.integer(n8$Iterations_1)
m <- fread("output/diffusion/10% starting_100% prob_70% threshold_1.csv")
m2 <- fread("output/diffusion/10% starting_100% prob_70% threshold_2.csv")
m3 <- fread("output/diffusion/10% starting_100% prob_70% threshold_3.csv")
m4 <- fread("output/diffusion/10% starting_100% prob_70% threshold_4.csv")
m5 <- fread("output/diffusion/10% starting_100% prob_70% threshold_5.csv")
m6 <- fread("output/diffusion/10% starting_100% prob_70% threshold_6.csv")
m7 <- fread("output/diffusion/10% starting_100% prob_70% threshold_7.csv")
m8 <- fread("output/diffusion/10% starting_100% prob_70% threshold_8.csv")

ggplot() +
  geom_point(data=n,aes(x=Nodes, y=Iterations, color ='black')) +
  geom_point(data=n2,aes(x=Nodes, y=Iterations, color ='black')) +
  geom_point(data=n3,aes(x=Nodes, y=Iterations, color ='black')) +
  geom_point(data=n4,aes(x=Nodes, y=Iterations, color ='black')) +
  geom_point(data=n5,aes(x=Nodes, y=Iterations, color ='black')) +
  geom_point(data=n6,aes(x=Nodes, y=Iterations, color ='black')) +
  geom_point(data=n7,aes(x=Nodes, y=Iterations, color ='black')) +
  geom_point(data=n8,aes(x=Nodes, y=Iterations, color ='black')) +
  geom_point(data=n9,aes(x=Nodes, y=Iterations, color ='black')) +
  geom_point(data=n10,aes(x=Nodes, y=Iterations, color ='black')) +
  geom_point(data=m, aes(x=Nodes, y=Iterations_1, color ='red')) +
  geom_point(data=m2, aes(x=Nodes, y=Iterations_1, color ='red')) +
  geom_point(data=m3, aes(x=Nodes, y=Iterations_1, color ='red')) +
  geom_point(data=m4, aes(x=Nodes, y=Iterations_1, color ='red')) +
  geom_point(data=m5, aes(x=Nodes, y=Iterations_1, color ='red')) +
  geom_point(data=m6, aes(x=Nodes, y=Iterations_1, color ='red')) +
  geom_point(data=m7, aes(x=Nodes, y=Iterations_1, color ='red')) +
  geom_point(data=m8, aes(x=Nodes, y=Iterations_1, color ='red')) +
  scale_color_identity(name = "Diffusion Type",
                       breaks = c("black", "red"),
                       labels = c("Removed Loops", "Standard"),
                       guide = "legend")

# based on mean
mean1 <- fread("removed_loops/diffusion/consolidated/1% starting_50% prob_50% threshold.csv")
mean2 <- fread("removed_loops/diffusion/consolidated/1% starting_100% prob_50% threshold.csv")
mean3 <- fread("removed_loops/diffusion/consolidated/1% starting_100% prob_70% threshold.csv")

ggplot() +
  geom_line(data=mean1,aes(x=Nodes, y=Mean), color = "Black") +
  geom_line(data=mean2,aes(x=Nodes, y=Mean), color = "Red") +
  geom_line(data=mean3,aes(x=Nodes, y=Mean), color = "Blue") + scale_x_log10() + scale_y_log10()


### compoaring results of random, removed, removed_loops and standard diffusion means
random <- fread("output/diffusion/consolidated/consolidated_random/10% starting_100% prob_70% threshold.csv")
removed_loops <- fread("removed_loops/diffusion/consolidated/10% starting_100% prob_70% threshold.csv")
removed <- fread("output/diffusion/consolidated/consolidated_removed/10% starting_100% prob_70% threshold.csv")
removed <- removed[removed$Name %in% random$Name]
standard <- fread("output/diffusion/consolidated/10% starting_100% prob_70% threshold.csv")
standard <- standard[standard$Name %in% random$Name]

results <- data.table(random[, 1:4], mean_random = random$Mean, mean_removed = removed$Mean,
                      mean_removed_loops = removed_loops$Mean, mean_standard = standard$Mean)

results <- na.omit(results)

ggplot() +
  geom_point(data=results, aes(x=Nodes, y=mean_random, color ="red")) +
  geom_point(data=results, aes(x=Nodes, y=mean_removed, color="blue")) +
  geom_point(data=results, aes(x=Nodes, y=mean_removed_loops, color="green")) +
  geom_point(data=results, aes(x=Nodes, y=mean_standard, color="black")) +
  scale_color_identity(name = "Diffusion Type",
                       breaks = c("red", "blue", "green", "black"),
                       labels = c("Random", "Removed", "Removed Loops", "Standard"),
                       guide = "legend")


######## comparing diffusion regression ########
# load in measures data
removed <- fread("removed_loops/output/master_measures_removed_loops.csv")
removed <- na.omit(removed)
master_data <- fread("output/undirected/master_measures_2.csv")
master_data <- master_data[master_data$Name %in% removed$Name]

table(removed$NetworkDomain)
# load in diffusion results
diff_removed <- fread("removed_loops/diffusion/consolidated/1% starting_50% prob_50% threshold.csv")
diff_removed <- diff_removed[diff_removed$Name %in% removed$Name]
diff_removed2 <- fread("removed_loops/diffusion/consolidated/1% starting_100% prob_50% threshold.csv")
diff_removed2 <- diff_removed2[diff_removed2$Name %in% removed$Name]
diff_removed3 <- fread("removed_loops/diffusion/consolidated/1% starting_100% prob_70% threshold.csv")
diff_removed3 <- diff_removed3[diff_removed3$Name %in% removed$Name]
diff <- fread("output/diffusion/consolidated/10% starting_100% prob_70% threshold.csv")
diff <- diff[diff$Name %in% diff_removed$Name]

# add mean to measure table
removed$Mean <- diff_removed$Mean
removed$Mean2 <- diff_removed2$Mean
removed$Mean3 <- diff_removed3$Mean
master_data$Mean <- diff$Mean



str(removed)
glm_removed <- lm(cbind(Mean, Mean2, Mean3) ~  GiniCloseness + MeanConstraint + GiniConstraint +
                     MedianDegree + GiniDegreeCount + GiniTransitivity +
                     AverageDegree + AveragePathLength +
                     AverageTransitivity + BetweennessCentrality + Closeness + ClosenessCentrality +
                     Complexity + DegreeAssortativity + DegreeCentrality + DegreeDistribution +
                     Density + EigenvectorCentrality + EigenvectorCentrality_2 + Entropy +
                     GlobalTransitivity + Nodes + Edges, data = removed)

b <- aov(glm_removed)
TukeyHSD(b)
summary(b)
install.packages("plm")
summary(glm_removed)
anova(glm_removed)
library(car)
Anova(glm_removed)
manova(glm_removed)
Manova(glm_removed)


glm_removed2 <- glm(Mean2 ~  constraint_mean + constraint_gini +
                      Median_degree + gini_t.Gini + gini_d.Gini +
                      AverageDegree + AveragePathLength +
                      AverageTransitivity + BetweennessCentrality + Closeness + ClosenessCentrality +
                      Complexity + DegreeAssortativity + DegreeCentrality + DegreeDistribution +
                      Density + EigenvectorCentrality + EigenvectorCentrality_2 + Entropy +
                      GlobalTransitivity + Nodes + Edges, data = removed)
summary(glm_removed2)

glm <- glm(Mean ~  AverageDegree + AveragePathLength +
             AverageTransitivity + BetweennessCentrality + Closeness + ClosenessCentrality +
             Complexity + DegreeAssortativity + DegreeCentrality + DegreeDistribution +
             Density + EigenvectorCentrality + EigenvectorCentrality_2 + Entropy +
             GlobalTransitivity + Nodes + Edges, data = master_data)

summary(glm_removed)
summary(glm_removed2)

ggplot(removed, aes(x = Closeness, y = DegreeCentrality)) + geom_point()
## Significant (without controlling for size)
# Average Transitivity ***
# Betweenness Centrality **
# Degree Distribution *
# Density **
# Eigenvector Centrality 2 ***
# Global Transitivity **

## Significant (controlling for Nodes)
# Average Degree *
# Average Transitivity *
# Betweenness Centrality .
# Density **
# Eigenvector Centrality *
# Global Transitivity ***
# (Nodes ***)

## Significant (controlling for Edges)
# Average Degree ***
# Average Path Length .
# Average Transitivity *
# Betwenness Centrality *
# Degree Distribution **
# Density ***
# Eigenvector Cnetrality 2 **
# (Edges ***)


summary(glm)

## Significant (without controlling for size)
# Average Transitivity ***
# Betweenness Centrality **
# Degree Distribution .
# Eigenvector Centrality 2 *
# Entropy .
# Global Transitivity *

## Significant (controlling for Nodes)
# Average Degree *
# Degree Centrlity .
# Global Transitivity **
# (Nodes ***)

## Significant (controlling for Edges)
# Average Degree ***
# Betwenness Centrality .
# Closeness .
# Closeness Centrality **
# Degree Centrality ***
# Degree Distribution *
# Density *
# Eigenvector Cnetrality 2 *
# (Edges ***)




















removed <- fread("removed_loops/output/master_measures_removed_loops.csv")
removed <- na.omit(removed)

length(unique(substr(removed$Name, 1, 4)))
length(unique(substr(removed$Name, 1, 5)))
length(unique(substr(removed$Name, 1, 6)))
length(unique(substr(removed$Name, 1, 7)))
length(unique(substr(removed$Name, 1, 8)))
length(unique(substr(removed$Name, 1, 9)))
length(unique(substr(removed$Name, 1, 10)))
