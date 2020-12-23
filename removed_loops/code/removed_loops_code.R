############################
# install packages
list.of.packages <- c("data.table", "igraph", "jsonlite", "ggplot2")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

# create removed_loops networks

master <- fread("output/undirected/master_measures_2.csv")
for(k in 243:513){
  net <- fread(sprintf("data/all_data/%s.csv", master[k, Name]))
  name <- master[k, Name]
  net <- unique(net)
  t <- 1
  n <- c()
  for(i in 1:nrow(net)){
    if(net[i, Node1] %in% net$Node2){
      c <- which(net$Node2 %in% net[i, Node1])
      for(j in c){
        if(net[j, Node1] == net[i, Node2]){
          n[t] <- j
          t <- t + 1
        }
      }
    }
  }
  if(is.null(n)){
    write.table(net, file = sprintf("removed_loops_data/%s", name), row.names = F, sep = ",")
  } else {
    net <- net[-n]
    write.table(net, file = sprintf("removed_loops_data/%s", name), row.names = F, sep = ",")
  }
  rm(net)
}

# removed_loops input table
master <- fread("output/undirected/master_measures_2.csv")
for(i in 1:242){
  net <- fread(sprintf("removed_loops/data/%s.csv", master[i, Name]))
  name <- master[i, Name]
  domain <- master[i, NetworkDomain]
  nodes <- length(unique(c(net$Node1, net$Node2)))
  edges <- nrow(net)
  if(i == 1){
    write.table(data.table(Name = name, NetworkDomain = domain, Nodes = nodes, Edges = edges),
                file = "removed_loops/output/removed_loops_table.csv", sep = ",", col.names = TRUE, row.names = FALSE)
  } else {
    write.table(data.table(Name = name, NetworkDomain = domain, Nodes = nodes, Edges = edges),
                file = "removed_loops/output/removed_loops_table.csv", sep = ",", col.names = FALSE, row.names = FALSE,
                append = TRUE)
  }
  rm(net)
}

# run measures
source("R/removed_loops_measures_function.R")
data <- fread("removed_loops/output/removed_loops_table.csv")

for(i in 128:nrow(data)){
  net <- fread(sprintf("removed_loops/%s.csv", data[i, Name]))
  removed.loops.network.measures(net, i)
  rm(net)
}



removed <- fread("removed_loops/output/removed_loops_measures.csv")
master_data <- fread("output/undirected/master_measures_2.csv")[1:242]


names(master)

ggplot() +
  geom_line(data=removed,aes(x=Nodes, y=AverageDegree), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=AverageDegree), color='red')

ggplot() +
  geom_line(data=removed,aes(x=Nodes, y=AverageTransitivity), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=AverageTransitivity), color='red')

ggplot() +
  geom_line(data=removed,aes(x=Nodes, y=BetweennessCentrality), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=BetweennessCentrality), color='red')

ggplot() +
  geom_line(data=removed,aes(x=Nodes, y=Closeness), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=Closeness), color='red')

ggplot() +
  geom_line(data=removed,aes(x=Nodes, y=ClosenessCentrality), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=ClosenessCentrality), color='red')

ggplot() +
  geom_line(data=removed,aes(x=Nodes, y=DegreeAssortativity), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=DegreeAssortativity), color='red')

ggplot() +
  geom_line(data=removed,aes(x=Nodes, y=DegreeCentrality), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=DegreeCentrality), color='red')

ggplot() +
  geom_line(data=removed,aes(x=Nodes, y=DegreeDistribution), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=DegreeDistribution), color='red')

ggplot() +
  geom_point(data=removed[Density < 0.2],aes(x=Nodes, y=Density), color='green') +
  geom_point(data=master_data[Density < 0.2], aes(x=Nodes, y=Density), color='red')

ggplot() +
  geom_line(data=removed,aes(x=Nodes, y=EigenvectorCentrality), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=EigenvectorCentrality), color='red')

ggplot() +
  geom_line(data=removed,aes(x=Nodes, y=EigenvectorCentrality_2), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=EigenvectorCentrality_2), color='red')

ggplot() +
  geom_line(data=removed,aes(x=Nodes, y=GlobalTransitivity), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=GlobalTransitivity), color='red')


































