########################
######## install packages ########
list.of.packages <- c("data.table", "igraph", "jsonlite", "ggplot2", "gridExtra", "mlbench",
                      "caret", "e1071", "corrplot")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

########################
######## comparing number of edges and nodes before and after removing loops ########
master_data <- fread("removed_loops/output/master_measures_removed_loops.csv")

## split domains into Social and Non-Social
for(i in 1:nrow(master_data)){
  if(master_data[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master_data[i, "NetworkDomain"] <- gsub(".*", "Social", master_data[i, "NetworkDomain"])
  } else {
    master_data[i, "NetworkDomain"] <- gsub(".*", "Non-Social", master_data[i, "NetworkDomain"])
  }
}

master_data$NetworkDomain <- as.factor(master_data$NetworkDomain)


# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=5)
# train the model
model_master_data <- glm(NetworkDomain ~ GiniDegreeCount +
                             GiniEigenvectorCentrality + GiniCloseness +
                             MedianDegree + AveragePathLength + Complexity +
                             BetweennessCentrality + ClosenessCentrality +
                             Complexity + DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
                             Density + GlobalTransitivity + Nodes,
                       data=master_data, family = "binomial")

summary(model_master_data)
model_master_data2 <- train(
  form = NetworkDomain ~ GiniDegreeCount +
    GiniEigenvectorCentrality + GiniCloseness +
    MedianDegree + AveragePathLength + Complexity +
    BetweennessCentrality + ClosenessCentrality +
    Complexity + DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
    Density + GlobalTransitivity + Nodes,
  data = master_data,
  trControl = control,
  preProcess="scale", method = "glm",
  family = "binomial"
)

# estimate variable importance
importance_master_data <- varImp(model_master_data, scale=T)
importance_master_data2 <- varImp(model_master_data2, scale=FALSE)
# summarize importance
print(importance_master_data)
print(importance_master_data2)
# plot importance
a <- plot(importance_master_data)
b <- plot(importance_master_data2)
grid.arrange(a, b)

# compute feautre importance with several steps
for(i in 1:1000){
  control <- rfeControl(functions=rfFuncs, method="cv", number=10)
  results_master_data <- rfe(master_data[,4:28], master_data[[3]], sizes=c(1:4), rfeControl=control)
  imp <- predictors(results_master_data)
  if(i == 1){
    write.table(data.table(Rank1 = imp[1], Rank2 = imp[2], Rank3 = imp[3], Rank4 = imp[4],
                           Rank5 = imp[5]),
                file = "removed_loops/output/feature_rank.csv", sep = ",",
                row.names = F)
  } else {
    write.table(data.table(imp[1], imp[2], imp[3], imp[4], imp[5]),
                file = "removed_loops/output/feature_rank.csv",
                row.names = F, sep = ",", col.names = F, append = T)
  }
}
?rfe
summary()
ranked <- fread("removed_loops/output/feature_rank.csv")
ranked$ID <- 1:nrow(ranked)
master <- melt(ranked, id.vars = "ID", variable.name = "category",
               value.name="scores")

ggplot(master, aes(x = category, fill = scores)) + geom_bar(position = "fill") + ylab("Frequency") +
  xlab("") + labs(fill = "Measure")


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
                       labels = c("master_data Loops", "Standard"),
                       guide = "legend")

# based on mean
mean1 <- fread("removed_loops/diffusion/consolidated/1% starting_50% prob_50% threshold.csv")
mean2 <- fread("removed_loops/diffusion/consolidated/1% starting_100% prob_50% threshold.csv")
mean3 <- fread("removed_loops/diffusion/consolidated/1% starting_100% prob_70% threshold.csv")

ggplot() +
  geom_line(data=mean1,aes(x=Nodes, y=Mean), color = "Black") +
  geom_line(data=mean2,aes(x=Nodes, y=Mean), color = "Red") +
  geom_line(data=mean3,aes(x=Nodes, y=Mean), color = "Blue") + scale_x_log10() + scale_y_log10()



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


















master_data <- fread("removed_loops/output/master_measures_removed_loops.csv")
master_data <- na.omit(master_data)

length(unique(substr(master_data$Name, 1, 4)))
length(unique(substr(master_data$Name, 1, 5)))
length(unique(substr(master_data$Name, 1, 6)))
length(unique(substr(master_data$Name, 1, 7)))
length(unique(substr(master_data$Name, 1, 8)))
length(unique(substr(master_data$Name, 1, 9)))
length(unique(substr(master_data$Name, 1, 10)))
