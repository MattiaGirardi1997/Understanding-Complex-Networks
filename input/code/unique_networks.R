##########################################
## Unique data
## Mattia Girardi
## 16.11.2020
#########################################

# set working directory
setwd("~/Desktop/Bachelor Thesis/code/bachelor_thesis")

# install packages
list.of.packages <- c("data.table", "corrplot", "ggplot2", "igraph")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

# load in complete measures
measures <- fread("output/undirected/master_measures_2.csv")

unique_list <- data.frame()
for(i in 1:length(measures$Name)){
  network <- substr(measures[i, Name], start = 1, stop = 6)
  domain <- measures[i, NetworkDomain]
  unique_list <- rbind(unique_list, data.frame(network, domain))
}

measures_unique <- distinct(unique_list)

write.table(measures_unique, file = "input/import_datasets/unique_networks.csv", sep = ",", row.names = F)

unique_networks <- fread("input/import_datasets/unique_networks.csv")

table(unique_networks$domain)

g <- fread("data/netzschleuder_Data/microsoft_concept.csv")

h <- graph_from_data_frame(g, directed = F)


source("R/netzschleuder_functions.R")

netzschleuder_essentials <- fread("input/netzschleuder_essentials2.csv")
netzschleuder_files <- list.files(path = "data/NS", pattern="*.csv", full.names=T)
netzschleuder_names <- list.files(path = "data/NS", pattern="*.csv", full.names=F)c

length(netzschleuder_files)
for(i in 1:1){
  netzschleuder_network <- fread(sprintf("%s", netzschleuder_files[i]))
  igraph <- create.igraph.object.NS(netzschleuder_network)
  measures <- compute.NS.measures(igraph, i, netzschleuder_data = fread("input/netzschleuder_essentials2.csv"))
  append.NS.measures(measures, i,  path = "output/undirected/NS.csv")
  rm(netzschleuder_network, igraph, measures)
}

i <- 1
name <- as.character(netzschleuder_names[i])
netzschleuder_network <- fread(sprintf("%s", netzschleuder_files[i]))
igraph <- create.igraph.object.NS(netzschleuder_network)
measures <- compute.NS.measures(igraph, i, netzschleuder_data = fread("input/netzschleuder_essentials2.csv"))
append.NS.measures(measures, i,  path = "output/undirected/NS.csv")
rm(netzschleuder_network)
