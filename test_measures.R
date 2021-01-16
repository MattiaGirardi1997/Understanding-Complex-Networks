library(data.table)
library(igraph)
library(DescTools)
library(ggplot2)
data <- fread("removed_loops/output/master_measures_removed_loops.csv")
i <-1
net <- fread(sprintf("removed_loops/data/%s.csv", data[i, Name]))
g <- graph_from_data_frame(net, directed = F)


Gini(transitivity(g, type = "local"))
Gini(centr_betw(g, normalized = F)$res)
centr_betw(g, normalized = T)$res
Gini(centr_degree(g)$res)
centr_clo(g)$res
Gini(centr_eigen(g)$vector)
Gini(transitivity(g, type = "local"), na.rm = T)

### Constraint
data <- fread("removed_loops/output/removed_loops_table.csv")
for(i in 1:nrow(data)){
  name <- data[i, Name]
  net <- fread(sprintf("removed_loops/data/%s.csv", data[i, Name]))
  net <- graph_from_data_frame(net, directed = F)
  MeanConstraint <- mean(constraint(net))
  GiniConstraint <- Gini(constraint(net))
  if(i == 1){
    write.table(data.table(data[i, 1:4], MeanConstraint, GiniConstraint), file = "removed_loops/output/constraint.csv",
                row.names = F, sep = ",")
  } else {
    write.table(data.table(data[i, 1:4], MeanConstraint, GiniConstraint), file = "removed_loops/output/constraint.csv",
                row.names = F, sep = ",", col.names = F, append = T)
  }
  rm(net, MeanConstraint, GiniConstraint)
}

### Gini Coefficients
data <- fread("removed_loops/output/removed_loops_table.csv")
for(i in 1:nrow(data)){
  name <- data[i, Name]
  net <- fread(sprintf("removed_loops/data/%s.csv", data[i, Name]))
  net <- graph_from_data_frame(net, directed = F)
  GiniBetweenness <- Gini(centr_betw(net)$res)
  GiniCloseness <- Gini(centr_clo(net)$res)
  GiniDegreeCount <- Gini(degree(net))
  GiniEigenvectorCentrality <- Gini(centr_eigen(net)$vector)
  GiniTransitivity <- Gini(transitivity(net, type = "local"), na.rm = T)
  if(i == 1){
    write.table(data.table(data[i, 1:4], GiniBetweenness, GiniCloseness,
                           GiniDegreeCount, GiniEigenvectorCentrality,
                           GiniTransitivity), file = "removed_loops/output/gini_coefficients.csv",
                row.names = F, sep = ",")
  } else {
    write.table(data.table(data[i, 1:4], GiniBetweenness, GiniCloseness,
                           GiniDegreeCount, GiniEigenvectorCentrality,
                           GiniTransitivity), file = "removed_loops/output/gini_coefficients.csv",
                row.names = F, sep = ",", col.names = F, append = T)
  }
  rm(net, GiniBetweenness, GiniCloseness,
     GiniDegreeCount, GiniEigenvectorCentrality,
     GiniTransitivity)
}

### Median Degree
data <- fread("removed_loops/output/removed_loops_table.csv")
for(i in 1:nrow(data)){
  name <- data[i, Name]
  net <- fread(sprintf("removed_loops/data/%s.csv", data[i, Name]))
  net <- graph_from_data_frame(net, directed = F)
  MedianDegree <- median(degreee(net))
  if(i == 1){
    write.table(data.table(data[i, 1:4], MedianDegree), file = "removed_loops/output/median_degree.csv",
                row.names = F, sep = ",")
  } else {
    write.table(data.table(data[i, 1:4], MedianDegree), file = "removed_loops/output/median_degree.csv",
                row.names = F, sep = ",", col.names = F, append = T)
  }
  rm(net, MedianDegree)
}


### Modularity
data <- fread("removed_loops/output/removed_loops_table.csv")
for(i in 1:nrow(data)){
  name <- data[i, Name]
  net <- fread(sprintf("removed_loops/data/%s.csv", data[i, Name]))
  net <- graph_from_data_frame(net, directed = F)
  cb <- cluster_edge_betweenness(net, weights = NULL, directed = F)
  mem <- membership(cb)
  Modularity <- modularity(net, mem)
  if(i == 1){
    write.table(data.table(data[i, 1:4], Modularity), file = "removed_loops/output/modularity.csv",
                row.names = F, sep = ",")
  } else {
    write.table(data.table(data[i, 1:4], mod), file = "removed_loops/output/modularity.csv",
                row.names = F, sep = ",", col.names = F, append = T)
  }
  rm(net, cb, mem)
}

### Desctiptives
removed <- fread("removed_loops/output/gini_coefficients.csv")

for(i in 1:nrow(removed)){
  if(removed[i, NetworkDomain] == "Social,Offline"){
    removed[i, "NetworkDomain"] <- gsub(".*", "Offline", removed[i, "NetworkDomain"])
  } else if (removed[i, NetworkDomain] == "Social,Online") {
    removed[i, "NetworkDomain"] <- gsub(".*", "Online", removed[i, "NetworkDomain"])
  } else {
    removed[i, "NetworkDomain"] <- gsub(".*", "Non-Social", removed[i, "NetworkDomain"])
  }
}

ggplot(removed, aes(y = modularity, x = log(Nodes), color = NetworkDomain)) + geom_point()

ggplot() +
  geom_line(data = removed, aes(x = Nodes, y = GiniBetweenness), color = "Red") +
  geom_line(data = removed, aes(x = Nodes, y = GiniCloseness), color = "Blue") +
  geom_line(data = removed, aes(x = Nodes, y = GiniDegreeCount), color = "Yellow") +
  geom_line(data = removed, aes(x = Nodes, y = GiniEigenvectorCentrality), color = "Green") +
  geom_line(data = removed, aes(x = Nodes, y = GiniTransitivity), color = "Orange")



ggplot(removed, aes(y = constraint_mean, x = log(Nodes), color = NetworkDomain)) + geom_point()









