##########################################################
#### adding "High school dynamic contacts (2011-2012)" from networkrepository.com

buzznet <- fread("input/code/added_networks/buzznet.txt")
buzznet <- data.frame(Node1 = buzznet$Node2, Node2 = buzznet$Node1)

write.table(buzznet, file = "data/added_networks/buzznet.csv", row.names = F, sep = ",")

network <- data.frame(network_name = "buzznet", networkDomain = "Social,Online", directed = "FALSE",
                      number_edges = "2763066")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)


