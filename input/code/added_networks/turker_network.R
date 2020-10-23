##########################################################
#### adding "Amazon Mechanical Turkers (2016)" from Index of Colorado Networks

turker_network <- fread("input/code/added_networks/turker_network.csv")

write.table(turker_network, file = "data/added_networks/turker_network.csv", row.names = F, sep = ",")

network <- data.frame(network_name = "turker_network", networkDomain = "Social,Online", directed = "FALSE",
                      number_edges = "5268")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)






