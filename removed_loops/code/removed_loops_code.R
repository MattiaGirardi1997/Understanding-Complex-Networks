############################
# install packages
list.of.packages <- c("data.table", "igraph", "jsonlite", "ggplot2", "gridExtra",
                      "DescTools")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

# create removed_loops networks
master <- fread("removed_loops/output/master_measures_removed_loops.csv")
for(i in 1:nrow(master)){
  name <- master[i, Name]
  el <- fread(sprintf("data/all_data/%s.csv", name)) %>%
    graph_from_data_frame(directed = F) %>%
    simplify()
  el <- data.table(as_edgelist(decompose(el, min.vertices =
                                max(components(el)$csize))[[1]]))
  names(el) <- c("Node1", "Node2")
  el$Node1 <- as.integer(el$Node1)
  el$Node2 <- as.integer(el$Node2)
  write.table(el, file = sprintf("removed_loops/data/%s.csv",name), row.names = F, sep = ",")
  rm(el, name)
}

# removed_loops input table
master <- fread("removed_loops/output/master_measures_removed_loops.csv")
for(i in 1:nrow(master)){
  net <- fread(sprintf("removed_loops/data/%s.csv", master[i, Name]))
  name <- master[i, Name]
  domain <- master[i, NetworkDomain]
  nodes <- length(unique(c(net$Node1, net$Node2)))
  edges <- nrow(net)
  if(i == 1){
    write.table(data.table(Name = name, NetworkDomain = domain, Nodes = nodes, Edges = edges),
                file = "removed_loops/output/removed_loops_table2.csv", sep = ",", col.names = TRUE, row.names = FALSE)
  } else {
    write.table(data.table(Name = name, NetworkDomain = domain, Nodes = nodes, Edges = edges),
                file = "removed_loops/output/removed_loops_table2.csv", sep = ",", col.names = FALSE, row.names = FALSE,
                append = TRUE)
  }
  rm(net)
}


# run measures
source("R/removed_loops_measures_function.R")
data <- fread("removed_loops/output/removed_loops_table.csv")
for(i in 1:nrow(data)){
  net <- fread(sprintf("removed_loops/data/%s.csv", data[i, Name]))
  removed.loops.network.measures(net, i, path = "removed_loops/output/master_measures_removed_loops2.csv")
  rm(net)
}








