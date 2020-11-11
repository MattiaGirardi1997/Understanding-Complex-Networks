##########################################
## Main Script
## Mattia Girardi
## 20.09.2020
#########################################

# set working directory
setwd("~/Desktop/Bachelor Thesis/code/bachelor_thesis")

# install packages
list.of.packages <- c("data.table", "igraph", "jsonlite")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

####### ICON data
# load in functions
source("R/ICON_functions.R")

# load in ICON data
ICON_data <- fread("input/import_datasets/ICON_data.csv")

# compute network measure for ICON networks (26 networks)
for(i in 1:length(ICON_data[, network_name])){
  ICON_file <- ICON_data[i, network_name]
  load(sprintf("data/ICON_data/%s.rda", ICON_file))
  ICON.network.measures(eval(parse(text = sprintf("%s", ICON_file))), i, path = "output/undirected/ICON_measures_2.csv")
  objects_to_remove <- as.character(parse(text = sprintf("%s", ICON_file)))
  rm(list = c(objects_to_remove))
  rm(objects_to_remove)
}

rm(list = c("append.ICON.measures", "compute.ICON.measures",
            "create.igraph.object.ICON", "ICON.network.measures"))

####### Optimal Link Prediction data (510 networks)
# load in functions
source("R/OLP_functions.R")

# load in OLP data
OLP_files <- list.files(path = "data/OLP_data", pattern = "*.csv", full.names = TRUE)
OLP_essentials <- fread("input/import_datasets/OLP_essentials.csv")

# compute network measure for OLP networks
length(OLP_files)
for(i in 1:length(OLP_files)){
  OLP_network <- fread(OLP_files[i])
  OLP.network.measures(OLP_network, i, path = "output/undirected/OLP_measures_2.csv")
  rm(OLP_network)
}

rm(list = c("append.OLP.measures", "compute.OLP.measures", "convert.OLP",
            "create.igraph.object.OLP", "OLP.network.measures"))

####### Netzschleuder data (612 networks)
# load in functions
source("R/netzschleuder_functions.R")

# load in Netzschleuder data
netzschleuder_files <- list.files(path = "data/netzschleuder_data", pattern="*.csv", full.names=T)
netzschleuder_essentials <- fread("input/import_datasets/netzschleuder_essentials.csv")

# compute network measure for Netzschleuder networks 
for(i in 1:length(netzschleuder_files)){
  if(netzschleuder_essentials[i, number_edges] < 1000000){
  netzschleuder_network <- fread(netzschleuder_files[i])
  NS.network.measures(netzschleuder_network, i, path = "output/netzschleuder_measures.csv")
  rm(netzschleuder_network)
  } else {
  i = i + 1 
  }
}

for(i in 1:length(netzschleuder_essentials$network_name)){
  name <- as.character(netzschleuder_essentials[i, network_name])
  netzschleuder_network <- fread(sprintf("data/netzschleuder_data/%s.csv", name))
  NS.network.measures(netzschleuder_network, i, path = "output/undirected/netzschleuder_measures_2.csv")
  rm(netzschleuder_network)
}

rm(list = c("append.NS.measures", "compute.NS.measures", "convert.NS",
            "create.igraph.object.NS", "NS.network.measures"))

####### added networks 
# load in functions
source("R/ICON_functions.R")

# load in ICON data
added_networks <- fread("input/import_datasets/added_networks_Essentials.csv")
added_networks_files <- list.files(path = "data/added_networks", pattern="*.csv", full.names=TRUE)

# compute network measure for added networks (with ICON functions)
for(i in 1:length(added_networks_files)){
  added_network <- fread(added_networks_files[i])
  igraph.network <- create.igraph.object.ICON(added_network, i, ICON_data = fread("input/import_datasets/added_networks_essentials.csv"))
  measures <- compute.ICON.measures(igraph.network, i, ICON_data = fread("input/import_datasets/added_networks_essentials.csv"))
  append.ICON.measures(measures, i, path = "output/undirected/added_networks_measures.csv")
  rm(added_network, igraph.network, measures)
}

rm(list = c("append.ICON.measures", "compute.ICON.measures",
            "create.igraph.object.ICON", "ICON.network.measures"))






