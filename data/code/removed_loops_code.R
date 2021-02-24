############################
# install packages
list.of.packages <- c("data.table", "igraph", "jsonlite", "ggplot2", "gridExtra",
                      "DescTools")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

# removing some networks from data set
files <- list.files(path = "data/all_data", pattern = "*.csv", full.names = F)[-c(103:111, 226, 236,
                                                                                  286:299, 301:304, 306:310)]

# create removed and simplified networks
for(i in 1:length(files)){
  name <- files[i]
  name <- gsub(".csv", "", name)
  el <- fread(sprintf("data/all_data/%s.csv", name)) %>%
    graph_from_data_frame(directed = F) %>%
    simplify()
  el <- data.table(as_edgelist(decompose(el, min.vertices =
                                max(components(el)$csize))[[1]]))
  names(el) <- c("Node1", "Node2")
  el$Node1 <- as.integer(el$Node1)
  el$Node2 <- as.integer(el$Node2)
  write.table(el, file = sprintf("data/final_data/%s.csv",name), row.names = F, sep = ",")
  rm(el, name)
}

# create input table
files <- unlist(lapply(list.files(path = "data/final_data", pattern = "*.csv", full.names = F),
                       function(x) gsub(".csv", "", x)))
data <- fread("input/import_datasets/master_essentials.csv")
data <- data[which(data$Name %in% files)]

for(i in 1:length(files)){
  net <- fread(sprintf("data/final_data/%s.csv", files[i]))
  name <- data[Name == sprintf("%s", files[i]), Name]
  domain <- data[Name == sprintf("%s", files[i]), NetworkDomain]
  net <- graph_from_data_frame(net, directed = F)
  nodes <- length(V(net))
  edges <- length(E(net))
  if(i == 1){
    write.table(data.table(Name = name, NetworkDomain = domain, Nodes = nodes, Edges = edges),
                file = "output/network_specs.csv", sep = ",", col.names = TRUE, row.names = FALSE)
  } else {
    write.table(data.table(Name = name, NetworkDomain = domain, Nodes = nodes, Edges = edges),
                file = "output/network_specs.csv", sep = ",", col.names = FALSE, row.names = FALSE,
                append = TRUE)
  }
  rm(net)
}

# run measures
source("R/measure_function.R")
data <- fread("output/network_specs.csv")
for(i in 1:nrow(data)){
  net <- fread(sprintf("data/final_data/%s.csv", data[i, Name]))
  network.measures(net, i, path = "output/master_measures.csv")
  rm(net)
}






