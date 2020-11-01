##########################################################
#### adding "twitter_follower" from networkrepository.com

twitter_follower <- fread("input/code/added_networks/twitter_follower.mtx")
twitter_follower <- select(twitter_follower, Node1, Node2)

write.table(twitter_follower, file = "data/added_networks/twitter_follower.csv", sep = ",", row.names = F)

network <- data.frame(network_name = "twitter_follower", networkDomain = "Social,Online", directed = "FALSE",
                      number_edges = "713319")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)


