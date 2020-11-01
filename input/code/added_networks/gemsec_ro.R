##########################################################
#### adding "gemsec_ro" from networkrepository.com

gemsec_ro <- fread("input/code/added_networks/gemsec_ro.txt")

write.table(gemsec_ro, file = "data/added_networks/gemsec_ro.csv", sep = ",", row.names = F)

network <- data.frame(network_name = "gemsec_ro", networkDomain = "Social,Online", directed = "FALSE",
                      number_edges = "125826")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)
