

netzschleuder_measures <- fread("output/netzschleuder_measures.csv")

netzschleuder_essentials <- fread("input/import_datasets/netzschleuder_essentials.csv")

netzschleuder_essentials <- netzschleuder_essentials[order(netzschleuder_essentials$number_edges, decreasing = T)]
netzschleuder_essentials <- netzschleuder_essentials[43:574]
netzschleuder_essentials <- netzschleuder_essentials[order(netzschleuder_essentials$network_name)]

g <- data.frame(netzschleuder_essentials$network_name[1:505], netzschleuder_measures$Name)


netzschleuder_essentials <- netzschleuder_essentials[-c(282:307)]


g <- graph_from_data_frame(soc_net_comms_amazon, directed = F)

NS.network.measures(soc_net_comms_amazon, 510)
