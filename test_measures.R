library(data.table)
library(igraph)
library(DescTools)
library(ggplot2)
data <- fread("removed_loops/output/master_measures_removed_loops.csv")
i <- 1
net <- fread(sprintf("removed_loops/data/%s.csv", data[i, Name]))
g <- graph_from_data_frame(net, directed = F)
edge_density(g)

centr_betw(g)$centralization
Gini(degree.distribution(g))
max(degree(g))

edge_density(g, loops = F)
length(E(g))
V(g)

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

