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
length(ICON_data[, Var_name])
for(i in 1:4){
  ICON.file <- ICON_data[i, Var_name]
  ICON.network <- fread(sprintf("input/ICON_data/%s.csv", ICON.file))
  ICON.network.measures(ICON.network, i, path = "output/ICON_measures_dir2.csv")
  rm(ICON.network)
}

rm(list = c("append.ICON.measures", "compute.ICON.measures",
            "create.igraph.object.ICON", "ICON.network.measures"))

####### Optimal Link Prediction data (550 networks)
# load in functions
source("R/OLP_functions.R")

# load in OLP data
OLP.files <- list.files(path = "input/OLP_data", pattern = "*.csv", full.names = TRUE)

# compute network measure for OLP networks
length(OLP.files)
for(i in 1:length(OLP.files)){
  OLP.network <- fread(OLP.files[i])
  OLP.network.measures(OLP.network, i)
  rm(OLP.network)
}

rm(list = c("append.OLP.measures", "compute.OLP.measures", "convert.OLP",
            "create.igraph.object.OLP", "OLP.network.measures"))

####### Netzschleuder data
# load in functions
source("R/netzschleuder_functions.R")

# load in Netzschleuder data
netzschleuder.files <- list.files(path = "input/netzschleuder_data", pattern="*.csv", full.names=TRUE)
netzschleuder_data <- fromJSON("input/import_datasets/netzschleuder_names.json")

# compute network measure for Netzschleuder networks (641 networks)
for(i in 1:length(netzschleuder.files)){
  netzschleuder.network <- fread(netzschleuder.files[i])
  NS.network.measures(netzschleuder.network, i, path = "output/netzschleuder_measures.csv")
  rm(netzschleuder.network)
}

i <- 2
netzschleuder.network <- fread(netzschleuder.files[i])
NS.network.measures(netzschleuder.network, i, path = "output/netzschleuder_measures.csv")
rm(netzschleuder.network)

create.igraph.object.NS(netzschleuder.network)
compute.NS.measures(create.igraph.object.NS(netzschleuder.network), 2)
netzschleuder.files[4]
g <- as.data.frame(netzschleuder.files)

