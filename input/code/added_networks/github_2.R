##########################################################
#### adding "GitHub Social Network" from snap.stanford.edu

github_2 <- fread("input/code/added_networks/github.csv")
github_2 <- data.frame(Node1 = github$id_1, Node2 = github$id_2)

write.table(github, file = "data/added_networks/github_2.csv", row.names = F, sep = ",")

network <- data.frame(network_name = "github_2", networkDomain = "Social,Online", directed = "FALSE",
                      number_edges = "289003")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)
