##########################################################
#### adding "Email network (Uni. R-V, Spain, 2003)" from Index of Colorado Networks

email_network <- fread("input/code/added_networks/email_network.txt")

setnames(email_network, "V1", "Node1")
setnames(email_network, "V2", "Node2")

write.table(email_network[,1:2], file = "data/added_networks/email_network.csv", sep = " ", row.names = F)

network <- data.frame(network_name = "email_network", networkDomain = "Social,Online", directed = "TRUE",
                      number_edges = "10903")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)
