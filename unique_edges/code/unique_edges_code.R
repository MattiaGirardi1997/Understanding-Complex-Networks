# install packages
list.of.packages <- c("data.table", "igraph", "jsonlite")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)


# make unique networks
files <- list.files(path = "data/all_data", pattern = "*.csv", full.names = T)
names <- list.files(path = "data/all_data", pattern = "*.csv", full.names = F)
for(i in 1:513){
  net <- fread(files[i])
  name <- names[i]
  net <- unique(net)
  write.table(net, file = sprintf("unique/%s", name), row.names = F, sep = ",")
  rm(net)
}

# unique input table

master <- fread("output/undirected/master_measures_2.csv")
for(i in 1:513){
  net <- fread(sprintf("unique_edges/data/%s.csv", master[i, Name]))
  name <- master[i, Name]
  domain <- master[i, NetworkDomain]
  nodes <- length(unique(c(net$Node1, net$Node2)))
  edges <- nrow(net)
  if(i == 1){
    write.table(data.table(Name = name, NetworkDomain = domain, Nodes = nodes, Edges = edges),
                file = "unique_edges/output/unique_edges_table.csv", sep = ",", col.names = TRUE, row.names = FALSE)
  } else {
    write.table(data.table(Name = name, NetworkDomain = domain, Nodes = nodes, Edges = edges),
                file = "unique_edges/output/unique_edges_table.csv", sep = ",", col.names = FALSE, row.names = FALSE,
                append = TRUE)
  }
  rm(net)
}

# run measures
source("R/unique_network_measures_function.R")
data <- fread("unique_edges/output/unique_edges_table.csv")

for(i in 1:nrow(data)){
  name <- data[i, Name]
  net <- fread(sprintf("unique_edges/data/%s.csv", name))
  unique.network.measures(net, i)
  rm(net)
}



measures[c]
m <- c()
k <- 1

for(i in c){
  name <- measures[i, Name]
  net <- graph_from_data_frame(fread(sprintf("unique_edges/data/%s.csv", name)), directed = F)
  m[k] <- mean(degree(net))
  k <- k + 1
  rm(net)
}

master <- fread("unique/outpur/unique_edges_measures.csv")
master_data <- fread("output/undirected/master_measures_2.csv")[c]



names(master)

ggplot() +
  geom_line(data=master, aes(x=Nodes, y=AverageDegree), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=AverageDegree), color='red')

ggplot() +
  geom_line(data=master, aes(x=Nodes, y=AverageTransitivity), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=AverageTransitivity), color='red')

ggplot() +
  geom_line(data=master, aes(x=Nodes, y=BetweennessCentrality), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=BetweennessCentrality), color='red')

ggplot() +
  geom_line(data=master, aes(x=Nodes, y=Closeness), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=Closeness), color='red')

ggplot() +
  geom_line(data=master, aes(x=Nodes, y=ClosenessCentrality), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=ClosenessCentrality), color='red')

ggplot() +
  geom_line(data=master, aes(x=Nodes, y=DegreeAssortativity), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=DegreeAssortativity), color='red')

ggplot() +
  geom_line(data=master, aes(x=Nodes, y=DegreeCentrality), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=DegreeCentrality), color='red')

ggplot() +
  geom_line(data=master, aes(x=Nodes, y=DegreeDistribution), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=DegreeDistribution), color='red')

ggplot() +
  geom_line(data=master, aes(x=Nodes, y=Density), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=Density), color='red')

ggplot() +
  geom_line(data=master, aes(x=Nodes, y=EigenvectorCentrality), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=EigenvectorCentrality), color='red')

ggplot() +
  geom_line(data=master, aes(x=Nodes, y=EigenvectorCentrality_2), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=EigenvectorCentrality_2), color='red')

ggplot() +
  geom_line(data=master, aes(x=Nodes, y=GlobalTransitivity), color='green') +
  geom_line(data=master_data, aes(x=Nodes, y=GlobalTransitivity), color='red')


