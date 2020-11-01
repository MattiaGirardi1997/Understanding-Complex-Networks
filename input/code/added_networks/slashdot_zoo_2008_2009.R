##########################################################
#### adding "Slashdot Zoo (2008, 2009)" from Index of Colorado Networks

# slashdot_zoo_2008
slashdot_zoo_2008 <- fread("input/code/added_networks/slashdot_zoo_2008.txt")
slashdot_zoo_2008 <- data.frame(Node1 = slashdot_zoo_2008$`# FromNodeId`, Node2 = slashdot_zoo_2008$ToNodeId)

slashdot_zoo_2008$Node1 <- slashdot_zoo_2008$Node1 + 1
slashdot_zoo_2008$Node2 <- slashdot_zoo_2008$Node2 + 1

write.table(slashdot_zoo_2008, file = "data/added_networks/slashdot_zoo_2008.csv", row.names = F, sep = ",")

network <- data.frame(network_name = "slashdot_zoo_2008", networkDomain = "Social,Online", directed = "TRUE",
                      number_edges = "905468")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)

# slashdot_zoo_2009
slashdot_zoo_2009 <- fread("input/code/added_networks/slashdot_zoo_2009.txt")
slashdot_zoo_2009 <- data.frame(Node1 = slashdot_zoo_2009$`# FromNodeId`, Node2 = slashdot_zoo_2009$ToNodeId)

slashdot_zoo_2009$Node1 <- slashdot_zoo_2009$Node1 + 1
slashdot_zoo_2009$Node2 <- slashdot_zoo_2009$Node2 + 1

write.table(slashdot_zoo_2009, file = "data/added_networks/slashdot_zoo_2009.csv", row.names = F, sep = ",")

network <- data.frame(network_name = "slashdot_zoo_2009", networkDomain = "Social,Online", directed = "TRUE",
                      number_edges = "948464")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)









