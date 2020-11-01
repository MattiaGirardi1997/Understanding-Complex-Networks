##########################################################
#### adding "EU email communication network" from snap.stanford.edu

email_eu_all <- fread("input/code/added_networks/email_eu_all.txt", col.names = c("Node1","Node2"))

write.table(email_eu_all, file = "data/added_networks/email_eu_all.csv", row.names = F, sep = ",")

network <- data.frame(network_name = "email_eu_all", networkDomain = "Social,Online", directed = "TRUE",
                      number_edges = "420045")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)
