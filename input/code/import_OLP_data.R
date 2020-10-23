##########################################
## Importing OLP data
## Mattia Girardi
## 02.10.2020
#########################################

# set working directory
setwd("~/Desktop/Bachelor Thesis/code/bachelor_thesis")

# install packages
list.of.packages <- c("data.table", "BBmisc")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

##### create OLP data table with essential data
OLP_complete <- fread("input/import_datasets/OLP_complete.csv")[, c("network_name", "networkDomain", "title", "graphProperties",
                                                                    "number_edges")]

OLP_complete[, "network_name"] <- OLP_complete[, "title"]
OLP_complete[, "network_name"] <- gsub("_", " ", OLP_complete[, network_name])
OLP_complete[, "network_name"] <- gsub("_&", "", OLP_complete[, network_name])

write.table(OLP_complete, file = "input/import_datasets/OLP_essentials.csv", sep = ",", row.names = F)

rm(list=ls())

#### add indices to network names
OLP_essentials_sub <- fread("input/import_datasets/OLP_essentials.csv")

OLP_essentials_sub <- OLP_essentials_sub[order(OLP_essentials_sub$network_name), ]

unique_networks <- unique(OLP_essentials_sub[, "network_name"])
k <- 1
rm(i, j)
for(i in 1:length(unique_networks[, network_name])){
  network <- as.character(unique_networks[i])
  num <- length(agrep(network, OLP_essentials_sub$network_name))
  if(num == 1){
    k = k + 1
  } else if (i %in% c(50, 62)) {
    num <- length(grep(network, OLP_essentials_sub$network_name))
    for(j in 1:num){
      OLP_essentials_sub[k, "network_name"] <- gsub("$", sprintf("_%s", j), OLP_essentials_sub[k, network_name])
      k = k + 1
      rm(j)
    }
  } else if (i == 71) {
    num <- length(grep(network, OLP_essentials_sub$network_name)) - 1
    for(j in 1:num){
      OLP_essentials_sub[k, "network_name"] <- gsub("$", sprintf("_%s", j), OLP_essentials_sub[k, network_name])
      k = k + 1
      rm(j)
    }
  } else if (i %in% c(10, 20, 23, 25, 39, 45, 47, 52, 53, 60, 64, 65, 72, 77, 85, 86, 88)){
    k = k + 1
  } else {
    for(j in 1:num){
      OLP_essentials_sub[k, "network_name"] <- gsub("$", sprintf("_%s", j), OLP_essentials_sub[k, network_name])
      k = k + 1
      rm(j)
    }
  }
}

OLP_essentials <- OLP_essentials_sub[order(OLP_essentials_sub$network_name), ]
OLP_essentials[, "network_name"] <- gsub(" ", "_", OLP_essentials[, network_name])

write.table(OLP_essentials, file = "input/import_datasets/OLP_essentials.csv", row.names = F, sep = ",")

rm(list=ls())

#### extract direction information from graph properties
OLP_essentials <- fread("input/import_datasets/OLP_essentials.csv")
OLP_essentials[, "graphProperties"] <- gsub(",", "", OLP_essentials$graphProperties)

length(OLP_essentials[, graphProperties])
for(i in 1:length(OLP_essentials[, graphProperties])){
  if(length(grep("Directed", OLP_essentials[i, graphProperties],  OLP_essentials$graphProperties)) == 1){
    OLP_essentials[i, "graphProperties"] <- sub(".*", "TRUE", OLP_essentials[i, graphProperties])
  } else {
    OLP_essentials[i, "graphProperties"] <- sub(".*", "FALSE", OLP_essentials[i, graphProperties])
  }
}

OLP_essentials <- setnames(OLP_essentials, "graphProperties", "directed")
OLP_essentials <- OLP_essentials[, -("title")]
OLP_essentials$network_name <- tolower(OLP_essentials$network_name)

write.table(OLP_essentials, file = "input/import_datasets/OLP_essentials.csv", row.names = F, sep = ",")

rm(list=ls())

##########################
#### load in OLP data files

# load in function
source("R/OLP_functions.R")

#
OLP_data <- fread("input/import_datasets/OLP_complete.csv")[, c("title", "edges_id")]
OLP_data <- OLP_data[order(OLP_data$title), ]
OLP_essentials <- fread("input/import_datasets/OLP_essentials.csv")

OLP_data <- cbind(OLP_essentials[, "network_name"], OLP_data[, -c("title")])
OLP_data$network_name <- tolower(OLP_data$network_name)

for (i in 1:length(OLP_data[, edges_id])){
  OLP.network <- OLP_data[i, edges_id]
  OLP.converted <- convert.OLP(OLP.network)
  OLP.file <- as.character(OLP_data[i, "network_name"])
  write.table(OLP.converted, file = sprintf("data/OLP_data2/%s.csv", OLP.file),
              sep = ",", row.names = FALSE)
}

rm(list=ls())

#### deleting data after comparing datasets & eleting networks which do not represent true Social networks 
#### (i.e. animal  networks, movie networks etc.)
OLP_essentials <- fread("input/import_datasets/OLP_essentials.csv")
OLP_essentials <- OLP_essentials[-c(57, 74:99, 202,  209:215, 252, 528:531)]

# deleted "dolphin_social_network_(1994-2001)"
# deleted "freshwater_stream_webs"
# deleted "les_miserables_coappearances"
# deleted "malaria_var_dbla_hvr_networks"
# deleted "ncaa_college_football_2000"
# deleted "webkb_graphs_(1998)"

write.table(OLP_essentials, file = "input/import_datasets/OLP_essentials.csv", row.names = F, sep = ",")

#### classifying Social networks (Offline and Online); some changes done directly in file
OLP_essentials <- fread("input/import_datasets/OLP_essentials.csv")

OLP_essentials[c(217:328, 344), "networkDomain"] <- gsub("Economic", "Social,Offline",
                                                         OLP_essentials[c(217:328, 344), networkDomain])
OLP_essentials[c(329:343, 345:440), "networkDomain"] <- gsub("Social", "Social,Offline",
                                                         OLP_essentials[c(329:343, 345:440), networkDomain])

write.table(OLP_essentials, file = "input/import_datasets/OLP_essentials.csv", row.names = F, sep = ",")











