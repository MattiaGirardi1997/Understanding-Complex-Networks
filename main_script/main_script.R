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

# compute network measure for ICON networks (47 networks)
for(i in 1:length(ICON_data[, network_name])){
  ICON_file <- ICON_data[i, network_name]
  load(sprintf("data/ICON_data/%s.rda", ICON_file))
  ICON.network.measures(eval(parse(text = sprintf("%s", ICON_file))), i, path = "output/ICON_measures.csv")
  objects_to_remove <- as.character(parse(text = sprintf("%s", ICON_file)))
  rm(list = c("ICON_network", objects_to_remove))
  rm(objects_to_remove)
}

rm(list = c("append.ICON.measures", "compute.ICON.measures",
            "create.igraph.object.ICON", "ICON.network.measures"))

####### Optimal Link Prediction data (550 networks)
# load in functions
source("R/OLP_functions.R")

# load in OLP data
OLP_files <- list.files(path = "data/OLP_data", pattern = "*.csv", full.names = TRUE)

# compute network measure for OLP networks
length(OLP.files)
for(i in 1:length(OLP.files)){
  OLP.network <- fread(OLP_files[i])
  OLP.network.measures(OLP.network, i)
  rm(OLP.network)
}

rm(list = c("append.OLP.measures", "compute.OLP.measures", "convert.OLP",
            "create.igraph.object.OLP", "OLP.network.measures"))

####### Netzschleuder data (626 networks)
# load in functions
source("R/netzschleuder_functions.R")

# load in Netzschleuder data
netzschleuder_files <- list.files(path = "data/netzschleuder_data", pattern="*.csv", full.names=TRUE)
netzschleuder_data <- fromJSON("data/netzschleuder_data/netzschleuder_names.json")

# compute network measure for Netzschleuder networks 
for(i in 1:length(netzschleuder.files)){
  netzschleuder_network <- fread(netzschleuder_files[i])
  NS.network.measures(netzschleuder_network, i, path = "output/netzschleuder_measures.csv")
  rm(netzschleuder_network)
}

i <- 2
netzschleuder.network <- fread(netzschleuder.files[i])
NS.network.measures(netzschleuder.network, i, path = "output/netzschleuder_measures.csv")
rm(netzschleuder.network)

create.igraph.object.NS(netzschleuder.network)
compute.NS.measures(create.igraph.object.NS(netzschleuder.network), 2)
netzschleuder.files[4]
g <- as.data.frame(netzschleuder.files)

