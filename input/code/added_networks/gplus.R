##########################################################
#### adding "gplus" from networkrepository.com

gplus <- fread("input/code/added_networks/gplus.edges")

write.table(gplus, file = "data/added_networks/gplus.csv", sep = ",", row.names = F)

network <- data.frame(network_name = "gplus", networkDomain = "Social,Online", directed = "TRUE",
                      number_edges = "39242")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)

