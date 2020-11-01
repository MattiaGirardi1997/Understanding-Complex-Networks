##########################################################
#### adding "Signed networks in social media (2008, 2009)" from Index of Colorado Networks

#slashdot_1
slashdot_1 <- fread("input/code/added_networks/slashdot_1.txt")[, 1:2]

setnames(slashdot_1, "# FromNodeId", "Node1")
setnames(slashdot_1, "ToNodeId", "Node2")

write.table(slashdot_1, file = "data/added_networks/slashdot_1.csv", row.names = F, sep = ",")

network <- data.frame(network_name = "slashdot_1", networkDomain = "Social,Online", directed = "TRUE",
                      number_edges = "545671")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)

#slashdot_2
slashdot_2 <- fread("input/code/added_networks/slashdot_2.txt")[, 1:2]

setnames(slashdot_2, "# FromNodeId", "Node1")
setnames(slashdot_2, "ToNodeId", "Node2")

write.table(slashdot_2, file = "data/added_networks/slashdot_2.csv", row.names = F, sep = ",")

network <- data.frame(network_name = "slashdot_2", networkDomain = "Social,Online", directed = "TRUE",
                      number_edges = "549202")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)

#slashdot_3
slashdot_3 <- fread("input/code/added_networks/slashdot_3.txt")[, 1:2]

setnames(slashdot_3, "# FromNodeId", "Node1")
setnames(slashdot_3, "ToNodeId", "Node2")

write.table(slashdot_3, file = "data/added_networks/slashdot_3.csv", row.names = F, sep = ",")

network <- data.frame(network_name = "slashdot_3", networkDomain = "Social,Online", directed = "TRUE",
                      number_edges = "516575")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)
