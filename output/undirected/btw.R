dnc <- fread("data/netzschleuder_data/dnc.csv")
seven <- fread("data/netzschleuder_data/7th_graders.csv")

# with igraph
library(igraph)
network <- graph_from_data_frame(dnc, directed = F)
sevenNet <- graph_from_data_frame(seven, directed = F)
g <- AdjacencyFromEdgelist(seven, check.full = T)
class(g)
as.matrix(g)

var(betweenness(network, normalized = T))
var(betweenness(sevenNet, normalized = T))

betweenness(AdjacencyFromEdgelist(seven, check.full = T))
var(betweenness(as.matrix(dnc), gmode = "graph", cmode = "undirected"))
scan("data/netzschleuder_data/7th_graders.csv")
seven$Weight <- 1


btw_igraph <- var(betweenness(network))
btw_igraph2 <- var(edge_betweenness(network))
f <- sna::as.edgelist.sna(g, as.digraph = F)
# with sna
detach("package:igraph", unload = TRUE)
library(sna)
network <- as.matrix(dnc)
btw_sna <- var(betweenness(network, cmode = "linearscaled"))
var(stresscent(as.matrix(seven), cmode = "undirected", gmode = "graph"))
?stresscent()
?betweenness


connectedness(as.matrix(dnc))







g <- AdjacencyFromEdgelist(dnc, check.full = T)$adjacency
dnc$Weight <- 1

betweenness(g)

Adjacen


M <- matrix(0, max(dnc[, 1:2]), max(dnc[, 1:2]))



adj <- edgelist_to_adjmat(dnc)
betweenness(adj)

library(igraph)
library(data.table)
library(dplyr)
library(ggplot2)

# load in Netzschleuder data
netzschleuder_files <- list.files(path = "data/netzschleuder_data", pattern="*.csv", full.names=T)
netzschleuder_essentials <- fread("input/import_datasets/netzschleuder_essentials.csv")

btw <- c()
names <- c()
domains <- c()
for(i in 155:168){
  name <- as.character(netzschleuder_essentials[i, network_name])
  names <- combine(names, name)
  domain <- as.character(netzschleuder_essentials[i, networkDomain])
  domains <- combine(domains, domain)
  netzschleuder_network <- fread(sprintf("data/netzschleuder_data/%s.csv", name))
  graph <- graph_from_data_frame(netzschleuder_network, directed = F)
  score <- var(betweenness(graph, normalized = T))
  btw <- combine(btw, score)
  rm(name, netzschleuder_network, graph, score)
  write.table(data.frame(names, domains, btw), file = "output/undirected/btw.csv", row.names = F, sep = ",")
}

g <- data.frame(names, domains, btw)


ggplot(g, aes(x = btw, fill = domains)) + geom_histogram()


# Warnmeldung:
#  In vc * vc : NAs durch Ganzzahlüberlauf erzeugt