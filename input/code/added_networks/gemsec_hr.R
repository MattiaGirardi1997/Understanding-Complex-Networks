##########################################################
#### adding "gemsec_hr" from networkrepository.com

gemsec_hr <- fread("input/code/added_networks/gemsec_hr.edges", col.names = c("Node1", "Node2"))

write.table(gemsec_hr, file = "data/added_networks/gemsec_hr.csv", sep = ",", row.names = F)

network <- data.frame(network_name = "gemsec_hr", networkDomain = "Social,Online", directed = "FALSE",
                      number_edges = "498202")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)

