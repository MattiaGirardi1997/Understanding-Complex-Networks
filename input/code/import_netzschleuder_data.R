##########################################
## Creating edgelist files for Netzschleuder networks
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
A <- c(8, 10, 11, 12, 15, 18, 23, 27, 32, 33, 44, 46:50, 52:63, 70, 72, 75:78, 80:87, 90, 105, 106, 107, 109, 113, 120, 123, 132, 133,
       137, 140, 141, 142, 144, 145, 148, 150, 151, 154, 155, 156, 159, 160, 163, 165, 167, 171, 173, 175, 177, 183, 186, 191, 195, 198, 199, 200, 201, 204, 206, 208:215, 218, 219, 221, 222, 223, 226, 227, 228, 229, 231,
       233, 235, 237, 239, 245, 247:254, 259, 261)

netzschleuder_names <- fromJSON("input/netzschleuder_data/netzschleuder_names.json")
netzschleuder_complete <- fromJSON("input/netzschleuder_data/netzschleuder_complete.json")

for (i in (1:length(netzschleuder_names))[-A]){
  if (length(netzschleuder_complete[[i]][["nets"]]) == 1){
    schleuder.network <- as.character(netzschleuder_names[i])
    temp <- tempfile()
    download.file(sprintf("https://networks.skewed.de/net/%s/files/%s.csv.zip", schleuder.network, schleuder.network),temp)
    data <- read.csv(unz(temp, "edges.csv"))[, 1:2]
    data[, 1] = data[, 1] + 1
    data[, 2] = data[, 2] + 1
    write.table(data[, 1:2], file = sprintf("input/Netzschleuder_data/%s.csv", schleuder.network),
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
      write.table(data[, 1:2], file = sprintf("input/Netzschleuder_data/%s_%s.csv", schleuder.network, schleuder.sub.network),
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
  write.table(data.table, file = sprintf("input/netzschleuder_data/domains/netzschleuder_%s.csv", file), sep = ",", row.names = F)
  rm(data.table, temp)
}

rm(list=ls())

#### seperate Social from Social,Online/Social,Offline
online <- fread("input/netzschleuder_data/domains/netzschleuder_Online.csv")
offline <- fread("input/netzschleuder_data/domains/netzschleuder_Offline.csv")
social <- fread("input/netzschleuder_data/domains/netzschleuder_Social.csv")

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

biological <- fread("input/netzschleuder_data/domains/netzschleuder_Biological.csv")
ecoomic <- fread("input/netzschleuder_data/domains/netzschleuder_Economic.csv")
informational <-fread("input/netzschleuder_data/domains/netzschleuder_Informational.csv")
technological <- fread("input/netzschleuder_data/domains/netzschleuder_Technological.csv")
transportation <- fread("input/netzschleuder_data/domains/netzschleuder_Transportation.csv")

netzschleuder_data <- rbind(social2, biological, ecoomic, informational, technological,
                            transportation, offline, online)

netzschleuder_data <- netzschleuder_data[order(netzschleuder_data$network_name), ]
netzschleuder_data[, "networkDomain"] <- gsub("Offline", "Social,Offline", netzschleuder_data[, networkDomain])
netzschleuder_data[, "networkDomain"] <- gsub("Online", "Social,Online", netzschleuder_data[, networkDomain])

write.table(netzschleuder_data, file = "input/import_datasets/netzschleuder_data.csv",
            sep = ",", row.names = FALSE)

rm(list=ls())

#### data table with file name, network domain and direction
A <- c(8, 10, 11, 12, 15, 18, 23, 27, 32, 33, 44, 46:50, 52:63, 70, 72, 75:78, 80:87, 90, 105, 106, 107, 109, 113, 120, 123, 132, 133,
       137, 140, 141, 142, 144, 145, 148, 150, 151, 154, 155, 156, 159, 160, 163, 165, 167, 171, 173, 175, 177, 183, 186, 191, 195, 198, 199, 200, 201, 204, 206, 208:215, 218, 219, 221, 222, 223, 226, 227, 228, 229, 231,
       233, 235, 237, 239, 245, 247:254, 259, 261)

netzschleuder_names <- fromJSON("input/netzschleuder_data/netzschleuder_names.json")
netzschleuder_names <- netzschleuder_names[-A]
netzschleuder_names_sub <- sub("_", " ", netzschleuder_names)

netzschleuder_data <- fread("input/import_datasets/netzschleuder_data.csv")
netzschleuder_data <- netzschleuder_data[-20]
netzschleuder_data <- netzschleuder_data[-A]

netzschleuder_complete <- fromJSON("input/netzschleuder_data/netzschleuder_complete.json")

netzschleuder_files <- as.vector(list.files(path = "input/netzschleuder_data", pattern="*.csv", full.names=F))
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

?trans

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
netzschleuder_essentials <- fread(("input/import_datasets/netzschleuder_essentials.csv"))
netzschleuder_files <- as.vector(list.files(path = "input/netzschleuder_data", pattern="*.csv", full.names=T))

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

#### Deleted networks from netzschleuder data

# ambassador 8
# arxiv_authors 10
# arxiv_citation 11
# arxiv_collab 12
# baseball 15
# bison 18
# board_directors 23
# cattle 27
# ceo_club 32
# chess 33
# crime 44
# dblp_author_paper 46
# dblp_cite 47
# dblp_coauthor 48
# dblp_coauthor_snap 49
# dblp_simpices 50
# dbpedia_country 52
# dbpedia_genre 53
# dpedia_link 54
# dbpedia_location 55
# dbpedia_occupation 56
# dbpedia_producer 57
# dbpedia_recordlabel 58
# dbpedia_starring 59
# dbpedia_team 60
# dbpedia_writer 61
# dbpedia_feature 62
# delicious 63
# dolphins 70
# dutch_critisism 72
# edit_wikibooks 75
# edit_wikinews 76
# edit_wikiquote 77
# edit_wiktionary 78
# elec 80
# elite 81
# email_company 82
# email_enron 83
# email_eu 84
# epinions 85
# epinions_trust 86
# escorts 87
# eu_procurements_alt 90
# football 105
# football_tsevans 106
# foursquare 107
# foursquare_global 109
# game_thrones 113
# hens 120
# hiv_transmission 123
# internet_top_pop 132
# jazz_collab 133
# kangaroo 137
# kidnappings 140
# lastfm 141
# lastfm_aminer 142
# lesmis 144
# libimseti 145
# livejournal_aminer 148
# lkml_reply 150
# lkml_thread 151
# macaques 154
# mag_geology_coauthor 155
# mag_history_coauthor 156
# marvel_partnerships 159
# marvel_universe 160
# mislove_osn_livejournal 163, 2
# mislove_osn_orkut 163, 3
# moreno_sheep 165
# movie galaxies 167
# netscience 171
# new_zealand_collab 173
# november17 175
# openstreetmap 177
# physics_collab 183
# plant_pol_vazquez 186
# prosper 191
# reality_mining 195
# revolution 198
# rhesus_monkey 199
# roadnet 200
# route views 201
# slashdot_threads 204
# soc_net_comms_friendster 206, 1
# soc_net_comms_livejournal 206, 2
# soc_net_comms_orkut 206, 3
# sp_highschool 208
# sp_highschool_new 209
# sp_hospital 210
# sp_hypertext 211
# sp_infectous 212
# sp_kenyan_households 213
# sp_office 214
# sp_primary_school 215
# swingers 218
# terrorists_911 219
# trackers 221
# train_terrorists 222
# trec 223
# twitter_15m 226
# twitter_2009 227
# twitter_events 228
# twitter_higgs_retweet 229, 2
# twitter_higgs_reply 229, 3
# twitter_higgs_menion 229, 4
# twitter_higgs_activity 229, 5
# twitter_social 231
# uni_email 233
# us_agencies 235
# us_congress 237
# us_roads 239
# wiki_rfa 245
# wiki_talk 247
# wiki_users 248
# wikiconflict 249
# wikipedia-en-talk 250
# wikipedia_growth 251
# wikipedia_link 252
# wikitree 253
# windsurfers 254
# yahoo_song 259
# zebras 261

A <- c(8, 10, 11, 12, 15, 18, 23, 27, 32, 33, 44, 46:50, 52:63, 70, 72, 75:78, 80:87, 90, 105, 106, 107, 109, 113, 120, 123, 132, 133,
       137, 140, 141, 142, 144, 145, 148, 150, 151, 154, 155, 156, 159, 160, 163, 165, 167, 171, 173, 175, 177, 183, 186, 191, 195, 198, 199, 200, 201, 204, 206, 208:215, 218, 219, 221, 222, 223, 226, 227, 228, 229, 231,
       233, 235, 237, 239, 245, 247:254, 259, 261)






