##########################################################
#### adding "Hamsterer" from networkrepository.com

hamsterer <- fread("input/code/added_networks/hamsterer.txt")

write.table(hamsterer, file = "data/added_networks/hamsterer.csv", sep = ",", row.names = F)

network <- data.frame(network_name = "hamsterer", networkDomain = "Social,Online", directed = "FALSE",
                      number_edges = "16630")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)
