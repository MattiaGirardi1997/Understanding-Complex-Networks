##########################################################
#### adding "gemsec_hu" from networkrepository.com

gemsec_hu <- fread("input/code/added_networks/gemsec_hu.edges", col.names = c("Node1", "Node2"))

write.table(gemsec_hu, file = "data/added_networks/gemsec_hu.csv", sep = ",", row.names = F)

network <- data.frame(network_name = "gemsec_hu", networkDomain = "Social,Online", directed = "FALSE",
                      number_edges = "222887")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)

