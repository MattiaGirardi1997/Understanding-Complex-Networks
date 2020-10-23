##########################################################
#### adding "High school dynamic contacts (2011-2012)" from Index of Colorado Networks

# highschool_2011
highschool_2011 <- fread("input/code/added_networks/highschool_2011.csv")
highschool_2011 <- data.frame(Node1 = highschool_2011$V2, Node2 = highschool_2011$V3)

write.table(highschool_2011, file = "data/added_networks/highschool_2011.csv", row.names = F, sep = ",")

network <- data.frame(network_name = "highschool_2011", networkDomain = "Social,Offline", directed = "FALSE",
                      number_edges = "28561")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)

# highschool_2012
highschool_2012 <- fread("input/code/added_networks/highschool_2012.csv")
highschool_2012 <- data.frame(Node1 = highschool_2012$V2, Node2 = highschool_2012$V3)

write.table(highschool_2012, file = "data/added_networks/highschool_2012.csv", row.names = F, sep = ",")

network <- data.frame(network_name = "highschool_2012", networkDomain = "Social,Offline", directed = "FALSE",
                      number_edges = "45047")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)












