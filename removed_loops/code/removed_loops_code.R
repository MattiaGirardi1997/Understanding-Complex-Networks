############################
# install packages
list.of.packages <- c("data.table", "igraph", "jsonlite", "ggplot2", "gridExtra")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

# create removed_loops networks

master <- fread("output/undirected/master_measures_2.csv")
for(k in 1:513){
  net <- fread(sprintf("data/all_data/%s.csv", master[k, Name]))
  name <- master[k, Name]
  net <- unique(net)
  net$ID <- 1:nrow(net)
  final <- unique(net)
  final$ID <- 1:nrow(net)
  for(i in 1:nrow(net)){
    if(net[i, Node1] %in% net$Node2){
      c <- which(net$Node2 %in% net[i, Node1])
      for(j in c){
        if(net[j, Node1] == net[i, Node2]){
          final <- final[!ID == j]
        }
      }
    }
  }
  write.table(final[, 1:2], file = sprintf("removed_loops/%s.csv", name), row.names = F, sep = ",")
  rm(net, final)
}

# removed_loops input table
master <- fread("output/undirected/master_measures_2.csv")
for(i in 1:513){
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
data <- fread("removed_loops/output/removed_loops_table2.csv")
for(i in 1:nrow(data)){
  net <- fread(sprintf("new/%s.csv", data[i, Name]))
  removed.loops.network.measures(net, i, path = "removed_loops/output/removed_loops_measures2.csv")
  rm(net)
}


########################
#### comparing results
removed <- fread("removed_loops/output/removed_loops_measures.csv")
removed <- na.omit(removed)
master_data <- fread("output/undirected/master_measures_2.csv")
master_data <- master_data[master_data$Name %in% removed$Name]

names(removed)
### Average Degree
ggplot() +
  geom_line(data=removed,aes(x=Nodes, y=AverageDegree), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=AverageDegree), color='red')
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = AverageDegree, color = NetworkDomain)) +
               geom_line(), ggplot(master_data, aes(x = Nodes, y = AverageDegree,
                                                    color = NetworkDomain)) + geom_line())

### Average Path Length
ggplot() +
  geom_line(data=removed,aes(x=Nodes, y=AveragePathLength), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=AveragePathLength), color='red')
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = AveragePathLength, color = NetworkDomain)) +
               geom_line(), ggplot(master_data, aes(x = Nodes, y = AveragePathLength,
                                                    color = NetworkDomain)) + geom_line())

### Average Transitivity
ggplot() +
  geom_line(data=removed,aes(x=Nodes, y=AverageTransitivity), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=AverageTransitivity), color='red')
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = AverageTransitivity, color = NetworkDomain)) +
               geom_line(), ggplot(master_data, aes(x = Nodes, y = AverageTransitivity,
                                                    color = NetworkDomain)) + geom_line())

### Betweenness Centrality
ggplot() +
  geom_line(data=removed,aes(x=Nodes, y=BetweennessCentrality), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=BetweennessCentrality), color='red')
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = BetweennessCentrality, color = NetworkDomain)) +
               geom_line(), ggplot(master_data, aes(x = Nodes, y = BetweennessCentrality,
                                                    color = NetworkDomain)) + geom_line())

### Closeness
ggplot() +
  geom_line(data=removed,aes(x=Nodes, y=Closeness), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=Closeness), color='red')
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = Closeness, color = NetworkDomain)) +
               geom_line(), ggplot(master_data, aes(x = Nodes, y = Closeness,
                                                    color = NetworkDomain)) + geom_line())

### Closeness Centrality
ggplot() +
  geom_line(data=removed,aes(x=Nodes, y=ClosenessCentrality), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=ClosenessCentrality), color='red')
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = ClosenessCentrality, color = NetworkDomain)) +
               geom_line(), ggplot(master_data, aes(x = Nodes, y = ClosenessCentrality,
                                                    color = NetworkDomain)) + geom_line())

### Degree Assortativity
ggplot() +
  geom_line(data=removed,aes(x=Nodes, y=DegreeAssortativity), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=DegreeAssortativity), color='red')
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = DegreeAssortativity, color = NetworkDomain)) +
               geom_line(), ggplot(master_data, aes(x = Nodes, y = DegreeAssortativity,
                                                    color = NetworkDomain)) + geom_line())

### Degree Centrality
ggplot() +
  geom_line(data=removed,aes(x=Nodes, y=DegreeCentrality), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=DegreeCentrality), color='red')
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = DegreeCentrality, color = NetworkDomain)) +
               geom_line(), ggplot(master_data, aes(x = Nodes, y = DegreeCentrality,
                                                    color = NetworkDomain)) + geom_line())

### Degree Distribution
ggplot() +
  geom_line(data=removed,aes(x=Nodes, y=DegreeDistribution), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=DegreeDistribution), color='red')
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = DegreeDistribution, color = NetworkDomain)) +
               geom_line(), ggplot(master_data, aes(x = Nodes, y = DegreeDistribution,
                                                    color = NetworkDomain)) + geom_line())

### Density
ggplot() +
  geom_line(data=removed,aes(x=Nodes, y=Density), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=Density), color='red')
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = Density, color = NetworkDomain)) +
               geom_line(), ggplot(master_data, aes(x = Nodes, y = Density,
                                                    color = NetworkDomain)) + geom_line())

### Eigenvector Centrality
ggplot() +
  geom_line(data=removed,aes(x=Nodes, y=EigenvectorCentrality), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=EigenvectorCentrality), color='red')
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = EigenvectorCentrality, color = NetworkDomain)) +
               geom_line(), ggplot(master_data, aes(x = Nodes, y = EigenvectorCentrality,
                                                    color = NetworkDomain)) + geom_line())

### Eigenvector Centrality 2
ggplot() +
  geom_line(data=removed,aes(x=Nodes, y=EigenvectorCentrality_2), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=EigenvectorCentrality_2), color='red')
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = EigenvectorCentrality_2, color = NetworkDomain)) +
               geom_line(), ggplot(master_data, aes(x = Nodes, y = EigenvectorCentrality_2,
                                                    color = NetworkDomain)) + geom_line())

### Global Transivity
ggplot() +
  geom_line(data=removed,aes(x=Nodes, y=GlobalTransitivity), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=GlobalTransitivity), color='red')
# based on domain
grid.arrange(ggplot(removed, aes(x = Nodes, y = GlobalTransitivity, color = NetworkDomain)) +
               geom_line(), ggplot(master_data, aes(x = Nodes, y = GlobalTransitivity,
                                                    color = NetworkDomain)) + geom_line())










n <- fread("output/diffusion/random/10% starting_100% prob_70% threshold_1.csv")
n2 <- fread("output/diffusion/random/10% starting_100% prob_70% threshold_2.csv")
n3 <- fread("output/diffusion/random/10% starting_100% prob_70% threshold_3.csv")
n4 <- fread("output/diffusion/random/10% starting_100% prob_70% threshold_4.csv")
n$Iterations <- as.integer(n$Iterations)
n2$Iterations <- as.integer(n2$Iterations)
n3$Iterations <- as.integer(n3$Iterations)
n4$Iterations <- as.integer(n4$Iterations)
m <- fread("output/diffusion/10% starting_100% prob_70% threshold_1.csv")
m2 <- fread("output/diffusion/10% starting_100% prob_70% threshold_2.csv")
m3 <- fread("output/diffusion/10% starting_100% prob_70% threshold_3.csv")
m4 <- fread("output/diffusion/10% starting_100% prob_70% threshold_4.csv")


ggplot() +
  geom_line(data=n,aes(x=V3, y=Iterations), color='Black') +
  geom_line(data=n2,aes(x=V3, y=Iterations), color='Black') +
  geom_line(data=n3,aes(x=Nodes, y=Iterations), color='Black') +
  geom_line(data=n4,aes(x=Nodes, y=Iterations), color='Black') +
  geom_line(data=m, aes(x=Nodes, y=Iterations_1), color = 'red') +
  geom_line(data=m2, aes(x=Nodes, y=Iterations_1), color='red') +
  geom_line(data=m3, aes(x=Nodes, y=Iterations_1), color='red') +
  geom_line(data=m4, aes(x=Nodes, y=Iterations_1), color='red')






























