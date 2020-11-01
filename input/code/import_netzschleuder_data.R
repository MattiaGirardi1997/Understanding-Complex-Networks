##########################################
## Importing Netzschleuder data
## Mattia Girardi
## 20.09.2020
#########################################

# set working directory
setwd("~/Desktop/Bachelor Thesis/code/bachelor_thesis")

# install packages
list.of.packages <- c("data.table", "igraph", "jsonlite", "dplyr", "httr")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

# read in data and covert to edgelist
A <- c(8, 10, 11, 12, 15, 18, 23, 27, 32, 33, 44:50, 52:63, 70, 72, 75:78, 80:87, 90, 105, 106, 107, 109, 113,
       120, 123, 132, 133, 137, 140, 141, 142, 144, 145, 148, 150, 151, 154, 155, 156, 159, 160, 163, 165, 167,
       171, 173, 175, 177, 183, 186, 191, 195, 198, 199, 200, 201, 202, 204, 206, 208:215, 218, 219, 221, 222,
       223, 226, 227, 228, 229, 231, 233, 235, 237, 239, 245, 247:254, 259, 261)

netzschleuder_names <- fromJSON("data/netzschleuder_data/netzschleuder_names.json")
netzschleuder_complete <- fromJSON("data/netzschleuder_data/netzschleuder_complete.json")

for (i in (1:length(netzschleuder_names))[-A]){
  if (length(netzschleuder_complete[[i]][["nets"]]) == 1){
    schleuder.network <- as.character(netzschleuder_names[i])
    temp <- tempfile()
    download.file(sprintf("https://networks.skewed.de/net/%s/files/%s.csv.zip", schleuder.network, schleuder.network),temp)
    data <- read.csv(unz(temp, "edges.csv"))[, 1:2]
    data[, 1] = data[, 1] + 1
    data[, 2] = data[, 2] + 1
    write.table(data[, 1:2], file = sprintf("data/netzschleuder_data/%s.csv", schleuder.network),
                sep = ",", row.names = FALSE, col.names = c("Node1", "Node2"))
    unlink(temp)
    rm(data)
  } else {
    for (j in 1:length(netzschleuder_complete[[i]][["nets"]])){
      schleuder.network <- as.character(netzschleuder_names[i])
      schleuder.sub.network <- as.character(netzschleuder_complete[[i]][[6]][j])
      temp <- tempfile()
      download.file(sprintf("https://networks.skewed.de/net/%s/files/%s.csv.zip", schleuder.network, schleuder.sub.network),temp)
      data <- read.csv(unz(temp, "edges.csv"))[, 1:2]
      data[, 1] = data[, 1] + 1
      data[, 2] = data[, 2] + 1
      write.table(data[, 1:2], file = sprintf("data/netzschleuder_data/%s_%s.csv", schleuder.network, schleuder.sub.network),
                  sep = ",", row.names = FALSE, col.names = c("Node1", "Node2"))
      unlink(temp)
      rm(data)
    }
  }
}

rm(list=ls())

#### create netzschleuder datatable with network domains
domains <- c("Social", "Online", "Offline", "Informational", "Technological", "Economic", "Biological",
             "Transportation")

for(i in 1:length(domains)){
  temp <- tempfile()
  file <- as.character(domains[i])
  download.file(sprintf("https://networks.skewed.de/api/nets?tags=%s", file), temp)
  data.table <- as.data.table(fromJSON(temp))
  data.table[, networkDomain := c(file)]
  setnames(data.table, "V1", "network_name")
  write.table(data.table, file = sprintf("data/netzschleuder_data/domains/netzschleuder_%s.csv", file), sep = ",", row.names = F)
  rm(data.table, temp)
}

rm(list=ls())

#### seperate Social from Social,Online/Social,Offline
online <- fread("data/netzschleuder_data/domains/netzschleuder_Online.csv")
offline <- fread("data/netzschleuder_data/domains/netzschleuder_Offline.csv")
social <- fread("data/netzschleuder_data/domains/netzschleuder_Social.csv")

Gonline <- as.vector(online$network_name)
Goffline <- as.vector(offline$network_name)

my_list_online <- c()
for(i in 1:length(Gonline)){
  new_element <- grep(Gonline[i], social$network_name)
  for(j in 1:length(new_element)){
    my_list_online[[length(my_list_online) + 1]] <- new_element[j]
  }
}

my_list_offline <- c()
for(i in 1:length(Goffline)){
  new_element <- grep(Goffline[i], social$network_name)
  for(j in 1:length(new_element)){
    my_list_offline[[length(my_list_offline) + 1]] <- new_element[j]
  }
}

social2 <- social[-c(my_list_offline, my_list_online)]

biological <- fread("data/netzschleuder_data/domains/netzschleuder_Biological.csv")
ecoomic <- fread("data/netzschleuder_data/domains/netzschleuder_Economic.csv")
informational <-fread("data/netzschleuder_data/domains/netzschleuder_Informational.csv")
technological <- fread("data/netzschleuder_data/domains/netzschleuder_Technological.csv")
transportation <- fread("data/netzschleuder_data/domains/netzschleuder_Transportation.csv")

netzschleuder_data <- rbind(social2, biological, ecoomic, informational, technological,
                            transportation, offline, online)

netzschleuder_data <- netzschleuder_data[order(netzschleuder_data$network_name), ]
netzschleuder_data[, "networkDomain"] <- gsub("Offline", "Social,Offline", netzschleuder_data[, networkDomain])
netzschleuder_data[, "networkDomain"] <- gsub("Online", "Social,Online", netzschleuder_data[, networkDomain])

write.table(netzschleuder_data, file = "input/import_datasets/netzschleuder_data.csv",
            sep = ",", row.names = FALSE)

rm(list=ls())

#### data table with file name, network domain and direction
A <- c(8, 10, 11, 12, 15, 18, 23, 27, 32, 33, 44:50, 52:63, 70, 72, 75:78, 80:87, 90, 105, 106, 107, 109, 113,
       120, 123, 132, 133, 137, 140, 141, 142, 144, 145, 148, 150, 151, 154, 155, 156, 159, 160, 163, 165, 167,
       171, 173, 175, 177, 183, 186, 191, 195, 198, 199, 200, 201, 202, 204, 206, 208:215, 218, 219, 221, 222, 223, 226,
       227, 228, 229, 231, 233, 235, 237, 239, 245, 247:254, 259, 261)

netzschleuder_names <- fromJSON("data/netzschleuder_data/netzschleuder_names.json")
netzschleuder_names <- netzschleuder_names[-A]
netzschleuder_names_sub <- sub("_", " ", netzschleuder_names)

netzschleuder_data <- fread("input/import_datasets/netzschleuder_data.csv")
netzschleuder_data <- netzschleuder_data[-203]
netzschleuder_data <- netzschleuder_data[-A]

netzschleuder_complete <- fromJSON("data/netzschleuder_data/netzschleuder_complete.json")

netzschleuder_files <- as.vector(list.files(path = "data/netzschleuder_data", pattern="*.csv", full.names=F))
netzschleuder_files <- sub(".csv", "", netzschleuder_files)
netzschleuder_files_sub <- sub("_", " ", netzschleuder_files)

domain_list <- c()
direction_list <- c()

for (i in ((1:145)[-c(75, 110, 111)])){
  if(length(grep(netzschleuder_names_sub[i], netzschleuder_files_sub, value=T)) > 1 & i != 13 & i != 69 & i != 95
     & i != 125 & i != 129 | i == 93){
    num <- length(grep(netzschleuder_names_sub[i], netzschleuder_files_sub, value=T))
    new <- rep(as.character(netzschleuder_data[i, "networkDomain"]), num)
    domain_list <- combine(domain_list, new)
    direction <- rep(netzschleuder_complete[[sprintf("%s", netzschleuder_names[i])]][["analyses"]][[1]][["is_directed"]], num)
    direction_list <- combine(direction_list, direction)
    rm(new, num, direction)
  } else {
    domain_list <- combine(domain_list, as.character(netzschleuder_data[i, "networkDomain"]))
    direction_list <- combine(direction_list, netzschleuder_complete[[sprintf("%s", netzschleuder_names[i])]]
                              [["analyses"]][["is_directed"]])
  }
}

domain_list <- domain_list[-c(346)]
direction_list <- direction_list[-c(346)]
netzschleuder_files <- netzschleuder_files[-c(395, 526, 527, 541, 542, 559, 560, 561, 570)]
netzschleuder_essentials <- data.frame(network_name = netzschleuder_files, networkDomain = domain_list,
                                       directed = direction_list)

write.table(netzschleuder_essentials, file = "input/import_datasets/netzschleuder_essentials.csv",
            row.names = F, sep = ",")

rm(list=ls())

##### add further netzschleuder networks
netzschleuder_essentials <- fread(("input/import_datasets/netzschleuder_essentials.csv"))

network_name <- as.character(c("inploid", "mislove_osn_flickr", "mislove_osn_youtube", "plant_pol_kato", "plant_pol_robertson",
                               "soc_net_comms_amazon", "soc_net_comms_dblp", "soc_net_comms_youtube", "twitter"))
networkDomain <- c("Social,Online", "Social,Online", "Social,Online", "Biological", "Biological", "Social,Online",
                   "Social,Online", "Social,Online", "Social,Online")
directed <- c("TRUE", "TRUE", "TRUE", "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "TRUE")

added_networks <- data.frame(network_name, networkDomain, directed)

netzschleuder_essentials <- rbind(netzschleuder_essentials, added_networks)
netzschleuder_essentials <- transform(netzschleuder_essentials, network_name = as.character(network_name))
netzschleuder_essentials <- netzschleuder_essentials[order(netzschleuder_essentials$network_name), ]


write.table(netzschleuder_essentials, file = "input/import_datasets/netzschleuder_essentials.csv",
            row.names = F, sep = ",")

rm(list=ls())

####
# Further adjustments made directly in the file
####

#### adding number of edges to essential netzschleuder data
netzschleuder_essentials <- fread("input/import_datasets/netzschleuder_essentials.csv")
netzschleuder_files <- as.vector(list.files(path = "data/netzschleuder_data", pattern="*.csv", full.names=T))

number_edges <- c()

for(i in 1:length(netzschleuder_files)){
  network <- fread(sprintf("%s", netzschleuder_files[i]))
  edges <- length(network[, Node1])
  number_edges[[length(number_edges) + 1]] <- edges
  rm(network)
}

netzschleuder_essentials <- cbind(netzschleuder_essentials, number_edges)
write.table(netzschleuder_essentials, file = "input/import_datasets/netzschleuder_essentials.csv",
            row.names = F, sep = ",")

rm(list=ls())

#### deleting data after comparing datasets
netzschleuder_essentials <- fread("input/import_datasets/netzschleuder_essentials.csv")
netzschleuder_essentials <- netzschleuder_essentials[-c(235, 238, 241, 244, 283, 285, 241:244, 404, 405, 520,
                                                        544)]

# deleted "celegans_2019_hermaphrodite_chemical" from Netzschleuder dataset
# deleted "celegans_2019_hermaphrodite_gap_junction" from Netzschleuder dataset
# deleted "celegans_2019_male_chemical" from Netzschleuder dataset
# deleted "celegans_2019_male_gap_junction" from Netzschleuder dataset
# deleted "dutch_school_klas12b-net-4m" from Netzschleuder dataset
# deleted "dutch_school_klas12b-net-3m" from Netzschleuder dataset
# deleted "freshmen_t2" from Netzschleuder dataset
# deleted "freshmen_t3" from Netzschleuder dataset
# deleted "freshmen_t5" from Netzschleuder dataset
# deleted "freshmen_t6" from Netzschleuder dataset
# deleted "karate_77" from Netzschleuder dataset
# deleted "karate_78" from Netzschleuder dataset
# deleted "macaque_neural" from Netzschleuder dataset
# deleted "polbooks" from Netzschleuder dataset

write.table(netzschleuder_essentials, file = "input/import_datasets/netzschleuder_essentials.csv",
            row.names = F, sep = ",")


# caida_as


#### deleting remaining bipartite networks
netzschleuder_essentials <- fread("input/import_datasets/netzschleuder_essentials.csv")
netzschleuder_essentials <- netzschleuder_essentials[-c(98, 103, 257, 267, 270, 271, 272, 273, 286,
                                                        299, 369, 392, 516, 518, 523, 527, 528, 539,
                                                        548, 589, 592, 601, 602)]

# bibsonomy 98
# bookcrossing 103
# citeulike 257
# corporate_directors 267
# digg_votes 270
# discogs_affiliation 271
# discogs_genre 272
# discogs_label 273
# eu_procurements 286
# flickr_groups 299
# github 369
# jester 392
# movielens_100k 516
# nematode_mammal 518
# paris_transportation 523
# plant_pol_kato 527
# plant_pol_robertson 528
# reuters 539
# stackoverflow 548
# unicodelang 589
# visualizeus 592
# wiki_article_words 601
# wiki_categories 602

write.table(netzschleuder_essentials, file = "input/import_datasets/netzschleuder_essentials.csv",
            row.names = F, sep = ",")

