############################
# install packages
list.of.packages <- c("data.table", "igraph", "jsonlite", "ggplot2", "gridExtra")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

# create removed_loops networks
master <- fread("output/undirected/master_measures_2.csv")
for(i in 1:513){
  name <- master[i, Name]
  el <- data.table(as_edgelist(fread(sprintf("data/all_data/%s.csv", name)) %>%
                                 graph_from_data_frame(directed = F) %>%
                                 simplify()))
  names(el) <- c("Node1", "Node2")
  el$Node1 <- as.integer(el$Node1)
  el$Node2 <- as.integer(el$Node2)
  write.table(el, file = sprintf("removed_loops/data/%s.csv",name), row.names = F, sep = ",")
  rm(el)
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
data <- fread("removed_loops/output/removed_loops_table.csv")
for(i in 1:nrow(data)){
  net <- fread(sprintf("removed_loops/data/%s.csv", data[i, Name]))
  removed.loops.network.measures(net, i, path = "removed_loops/output/master_measures_removed_loops.csv")
  rm(net)
}

### include modularity
data <- fread("removed_loops/output/removed_loops_table.csv")
for(i in 1:nrow(data)){
  name <- data[i, Name]
  net <- fread(sprintf("removed_loops/data/%s.csv", data[i, Name]))
  net <- graph_from_data_frame(net, directed = F)
  cb <- cluster_edge_betweenness(net, weights = NULL, directed = F)
  mem <- membership(cb)
  mod <- modularity(net, mem)
  if(i == 1){
    write.table(data.table(Name = name, Modularity = mod), file = "removed_loops/output/modularity.csv",
                row.names = F, sep = ",")
  } else {
    write.table(data.table(name, mod), file = "removed_loops/output/modularity.csv",
                row.names = F, sep = ",", col.names = F, append = T)
  }
  rm(net, cb, mem)
}

removed <- fread("removed_loops/output/modularity.csv")
removed$Nodes <- data$Nodes
removed$NetworkDomain <- data$NetworkDomain
ggplot(removed, aes(x = log(Nodes), y = Modularity, color = NetworkDomain)) + geom_point()


##### first try at computing removed loops networks without 'simplify()' function
# create removed_loops networks

#master <- fread("output/undirected/master_measures_2.csv")
#for(k in 1:513){
#  net <- fread(sprintf("data/all_data/%s.csv", master[k, Name]))
#  name <- master[k, Name]
#  net <- unique(net)
#  net$ID <- 1:nrow(net)
#  final <- unique(net)
#  final$ID <- 1:nrow(net)
#  for(i in 1:nrow(net)){
#    if(net[i, Node1] %in% net$Node2){
#      c <- which(net$Node2 %in% net[i, Node1])
#      for(j in c){
#        if(net[j, Node1] == net[i, Node2]){
#          final <- final[!ID == j]
#        }
#      }
#    }
#  }
#  write.table(final[, 1:2], file = sprintf("removed_loops/%s.csv", name), row.names = F, sep = ",")
#  rm(net, final)
#}












