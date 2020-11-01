##########################################################
#### adding "Twitch Social Networks" from snap.stanford.edu

# twitch_de
twitch_de <- fread("input/code/added_networks/twitch_de.csv")

write.table(twitch_de, file = "data/added_networks/twitch_de.csv", row.names = F, sep = ",")

network <- data.frame(network_name = "twitch_de", networkDomain = "Social,Online", directed = "FALSE",
                      number_edges = "153138")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)

# twitch_engb
twitch_engb <- fread("input/code/added_networks/twitch_engb.csv")

write.table(twitch_engb, file = "data/added_networks/twitch_engb.csv", row.names = F, sep = ",")

network <- data.frame(network_name = "twitch_engb", networkDomain = "Social,Online", directed = "FALSE",
                      number_edges = "35324")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)

# twitch_es
twitch_es <- fread("input/code/added_networks/twitch_es.csv")

write.table(twitch_es, file = "data/added_networks/twitch_es.csv", row.names = F, sep = ",")

network <- data.frame(network_name = "twitch_es", networkDomain = "Social,Online", directed = "FALSE",
                      number_edges = "59382")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)

# twitch_fr
twitch_fr <- fread("input/code/added_networks/twitch_fr.csv")

write.table(twitch_fr, file = "data/added_networks/twitch_fr.csv", row.names = F, sep = ",")

network <- data.frame(network_name = "twitch_fr", networkDomain = "Social,Online", directed = "FALSE",
                      number_edges = "112666")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)

# twitch_ptbr
twitch_ptbr <- fread("input/code/added_networks/twitch_ptbr.csv")

write.table(twitch_ptbr, file = "data/added_networks/twitch_ptbr.csv", row.names = F, sep = ",")

network <- data.frame(network_name = "twitch_ptbr", networkDomain = "Social,Online", directed = "FALSE",
                      number_edges = "31299")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)

# twitch_ru
twitch_ru <- fread("input/code/added_networks/twitch_ru.csv")

write.table(twitch_ru, file = "data/added_networks/twitch_ru.csv", row.names = F, sep = ",")

network <- data.frame(network_name = "twitch_ru", networkDomain = "Social,Online", directed = "FALSE",
                      number_edges = "37304")

write.table(network, file = "input/import_datasets/added_networks.csv", row.names = F, sep = ",",
            append = T, col.names = F)






