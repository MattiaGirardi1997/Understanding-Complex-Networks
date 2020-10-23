##########################################################
#### adding "Location-based social networks" from Index of Colorado Networks

gowalla <- fread("input/code/added_networks/gowalla_edges.txt")
gowalla <- data.frame(Node1 = gowalla$V1, Node2 = gowalla$V2)

gowalla$Node1 <- gowalla$Node1 + 1
gowalla$Node2 <- gowalla$Node2 + 1

write.table(gowalla, file = "data/added_networks/gowalla.csv", row.names = F, sep = ",")

network <- data.frame(network_name = "gowalla", networkDomain = "Social,Online", directed = "FALSE",
                      number_edges = "1900654")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)



brightkite <- fread("input/code/added_networks/brightkite_edges.txt")
brightkite <- data.frame(Node1 = brightkite$V1, Node2 = brightkite$V2)

brightkite$Node1 <- brightkite$Node1 + 1
brightkite$Node2 <- brightkite$Node2 + 1

write.table(brightkite, file = "data/added_networks/brightkite.csv", row.names = F, sep = ",")

network <- data.frame(network_name = "brightkite", networkDomain = "Social,Online", directed = "FALSE",
                      number_edges = "428156")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)






