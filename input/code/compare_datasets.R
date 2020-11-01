##########################################
## Comparing datasets
## Mattia Girardi
## 20.09.2020
#########################################

# set working directory
setwd("~/Desktop/Bachelor Thesis/code/bachelor_thesis")

# install packages
list.of.packages <- c("data.table")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

#### load in datasets
netzschleuder_essentials <- fread("input/import_datasets/netzschleuder_essentials.csv")
ICON_data <- fread("input/import_datasets/ICON_data.csv")
OLP_essentials <- fread("input/import_datasets/OLP_essentials.csv")
added_networks <- fread("input/import_datasets/added_networks.csv")

# extraxt part of name to apply grep
extract_names <- function(dataset){
  dataset <- as.vector(dataset$network_name)
  dataset <- gsub("_", " ", dataset)
  dataset <- str_extract(dataset, '[A-Za-z0-9]+')
}

netzschleuder <- extract_names(netzschleuder_essentials)
ICON <- extract_names(ICON_data)
OLP <- extract_names(OLP_essentials)

###### comparing datasets by network_name

#### comparing netzschleuder data to OLP data
NS_OLP <- c()

for(i in 1:length(netzschleuder)){
  if(length(grep(netzschleuder[i], OLP, value = T)) >= 1){
  num <- length(grep(netzschleuder[i], OLP, value = T))
  for(j in 1:num){
  NS_OLP[[length(NS_OLP) + 1]] <- grep(netzschleuder[i], OLP, value = T)[j]
  }
  } else {
    i = i + 1
  }
  NS_OLP <- unique(NS_OLP)
}

#### comparing netzschleuder data to ICON data
NS_ICON <- c()

for(i in 1:length(netzschleuder)){
  if(length(grep(netzschleuder[i], ICON, value = T)) >= 1){
    num <- length(grep(netzschleuder[i], ICON, value = T))
    for(j in 1:num){
      NS_ICON[[length(NS_ICON) + 1]] <- grep(netzschleuder[i], ICON, value = T)[j]
    }
  } else {
    i = i + 1
  }
  NS_ICON <- unique(NS_ICON)
}

#### comparing ICON data to OLP data
ICON_OLP <- c()

for(i in 1:length(ICON)){
  if(length(grep(ICON[i], OLP, value = T)) >= 1){
    num <- length(grep(ICON[i], OLP, value = T))
    for(j in 1:num){
      ICON_OLP[[length(ICON_OLP) + 1]] <- grep(ICON[i], OLP, value = T)[j]
    }
  } else {
    i = i + 1
  }
  ICON_OLP <- unique(ICON_OLP)
}

###### comparing datasets by number_edges
master <- rbind(netzschleuder_essentials, ICON_data, OLP_essentials)

master_duplicated <- master[duplicated(master$number_edges)]

master_duplicated <- sort(as.vector(master_duplicated$number_edges))

#### deleting data from ICON dataset
ICON_data <- fread("input/import_datasets/ICON_data.csv")
ICON_data <- ICON_data[-c(1, 7, 8 , 12, 14, 16, 17, 18, 19, 20, 22, 35, 37, 38, 39)]

ICON_data <- ICON_data[order(ICON_data$network_name)]

write.table(ICON_data, file = "input/import_datasets/ICON_data.csv", row.names = F, sep = ",")

# deleted "aishihik_intensity" from ICON dataset
# deleted "as_relation" from ICON dataset
# deleted "coldlke_intensity" from ICON dataset
# deleted "internet_routers" from ICON dataset
# deleted "mcgregor_intensity" from ICON dataset
# deleted "neural_elegans" from ICON dataset
# deleted "parsnip_prevalence" from ICON dataset
# deleted "parsnip_intensity" from ICON dataset
# deleted "plant_pollinator_arroyo" from ICON dataset
# deleted "plant_pollinator_bh" from ICON dataset
# deleted "power_grid" from ICON dataset
# deleted "ppi_yeast" from ICON dataset
# deleted "seed_disperse_beehler" from ICON dataset
# deleted "seed_disperse_snow" from ICON dataset
# deleted "smallwood_intensity" from ICON dataset

table(master$networkDomain)


netzschleuder_essentials <- netzschleuder_essentials[order(netzschleuder_essentials$number_edges)]
netzschleuder_essentials <- netzschleuder_essentials[-c(556:612)]


master <- rbind(netzschleuder_essentials, ICON_data, OLP_essentials, added_networks)
table(master$networkDomain)
table(master$directed)

length(master[directed == TRUE & networkDomain == "Biological", network_name])
length(master[directed == FALSE & networkDomain == "Biological", network_name])
length(master[directed == TRUE & networkDomain == "Social,Offline", network_name])
length(master[directed == FALSE & networkDomain == "Social,Offline", network_name])
length(master[directed == TRUE & networkDomain == "Social,Online", network_name])
length(master[directed == FALSE & networkDomain == "Social,Online", network_name])
length(master[directed == TRUE & networkDomain == "Informational", network_name])
length(master[directed == FALSE & networkDomain == "Informational", network_name])
length(master[directed == TRUE & networkDomain == "Transportation", network_name])
length(master[directed == FALSE & networkDomain == "Tansportation", network_name])
length(master[directed == TRUE & networkDomain == "Economic", network_name])
length(master[directed == FALSE & networkDomain == "Economic", network_name])


g <- fread("input/edges.csv")
h <- graph_from_data_frame(g, directed = T)

plot(h)
compute.OLP.measures(h, 10)
reciprocity(h)

OLP_complete <- fread("input/import_datasets/OLP_complete.csv")[, c(1:14)]



master <- master[order(master$network_name)]

