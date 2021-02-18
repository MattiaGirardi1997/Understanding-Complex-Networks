library(data.table)
library(igraph)
library(DescTools)
library(ggplot2)
data <- fread("removed_loops/output/master_measures_removed_loops.csv")
i <- 1
net <- fread(sprintf("removed_loops/data/%s.csv", data[i, Name]))
g <- graph_from_data_frame(net, directed = F)

eb <- edge.betweenness.community(g)

plot(eb, g)


data <- fread("removed_loops/output/master_measures_removed_loops.csv")
for(i in 1:nrow(data)){
  name <- data[i, Name]
  net <- fread(sprintf("removed_loops/data/%s.csv", data[i, Name]))
  net <- graph_from_data_frame(net, directed = F)
  if(i == 1){
    res <- data.table(data[i, 1:4], Connected = is.connected(net), Components = components(net)$no)
  } else {
    res <- rbind(res, data.table(data[i, 1:4], Connected = is.connected(net),
                                 Components = components(net)$no))
  }
}

length(which(res$Connected))

### Constraint
data <- fread("removed_loops/output/removed_loops_table.csv")
for(i in 1:nrow(data)){
  name <- data[i, Name]
  net <- fread(sprintf("removed_loops/data/%s.csv", data[i, Name]))
  net <- graph_from_data_frame(net, directed = F)
  MeanConstraint <- mean(constraint(net))
  GiniConstraint <- Gini(constraint(net))
  if(i == 1){
    write.table(data.table(data[i, 1:4], MeanConstraint, GiniConstraint),
                file = "removed_loops/output/constraint.csv",
                row.names = F, sep = ",")
  } else {
    write.table(data.table(data[i, 1:4], MeanConstraint, GiniConstraint),
                file = "removed_loops/output/constraint.csv",
                row.names = F, sep = ",", col.names = F, append = T)
  }
  rm(net, MeanConstraint, GiniConstraint)
}

### Modularity
data <- fread("removed_loops/output/removed_loops_table.csv")
for(i in 1:nrow(data)){
  name <- data[i, Name]
  net <- fread(sprintf("removed_loops/data/%s.csv", data[i, Name]))
  net <- graph_from_data_frame(net, directed = F)
  Modularity <- modularity(edge.betweenness.community(net))
  if(i == 1){
    write.table(data.table(data[i, 1:4], Modularity),
                file = "removed_loops/output/modularity.csv",
                row.names = F, sep = ",")
  } else {
    write.table(data.table(data[i, 1:4], Modularity),
                file = "removed_loops/output/modularity.csv",
                row.names = F, sep = ",", col.names = F, append = T)
  }
  rm(net, MeanConstraint, GiniConstraint)
}





tab <- data[which(!data$Name %in% master_data$Name), c("Name", "NetworkDomain")]
res[res$Name %in% tab$Name]

data$Name %in% master_data$Name










