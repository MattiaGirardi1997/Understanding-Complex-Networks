##########################################################
#### adding "Last.fm friendships (2007)" from Index of Colorado Networks

last_fm <- fread("input/code/added_networks/lastfm.txt")
last_fm <- data.frame(Node1 = last_fm$V1, Node2 = last_fm$V2)

write.table(last_fm, file = "data/added_networks/last_fm.csv", row.names = F, sep = ",")

network <- data.frame(network_name = "last_fm", networkDomain = "Social,Online", directed = "FALSE",
                      number_edges = "517759")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)



