##########################################
## Descriptive Analysis
## Mattia Girardi
## 01.11.2020
#########################################

# set working directory
setwd("~/Desktop/Bachelor Thesis/code/bachelor_thesis")

# install packages
list.of.packages <- c("data.table", "corrplot", "ggplot2")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

# load in measures
measures <- fread("output/master_measures.csv")

# deleting projection networks
measures <- measures[-c(97:101, 102:103, 188:190, 229, 392, 428, 430:541)]

# arxiv_astroph
# arxiv_condmat
# arxiv_grqc
# arxiv_hepph
# arxiv_hepht
# atlas_of_economic_complexity_export_network
# dutch_corporate_boards_(1976,_1996,_2001)
# freshmen_t0
# malaria_genes_combined
# network_coauthors
# norwegian_board_directors

# correlation matrix
cor <- cor(measures[, 4:14], use = "complete.obs")

corrplot(cor)

# 
ggplot(measures, aes(x = AverageDegree, fill = NetworkDomain)) + geom_histogram(position = "identity",
                                                                                     alpha = 0.5)
ggplot(measures, aes(x = AveragePathLength, fill = NetworkDomain)) + geom_histogram(position = "identity",
                                                                                     alpha = 0.5)
ggplot(measures, aes(x = AverageTransitivity, fill = NetworkDomain)) + geom_histogram(position = "identity",
                                                                                     alpha = 0.5)
#ggplot(measures, aes(x = Betweenness, fill = NetworkDomain)) + geom_histogram(position = "identity", alpha = 0.5)
                                                                                     
ggplot(measures, aes(x = Closeness, fill = NetworkDomain)) + geom_histogram(position = "identity",
                                                                                     alpha = 0.5)
ggplot(measures, aes(x = DegreeAssortativity, fill = NetworkDomain)) + geom_histogram(position = "identity",
                                                                                     alpha = 0.5)
ggplot(measures, aes(x = DegreeDistribution, fill = NetworkDomain)) + geom_histogram(position = "identity",
                                                                                     alpha = 0.5)
ggplot(measures, aes(x = Density, fill = NetworkDomain)) + geom_histogram(position = "identity",
                                                                                     alpha = 0.5)
ggplot(measures, aes(x = EigenvectorCentrality, fill = NetworkDomain)) + geom_histogram(position = "identity",
                                                                                     alpha = 0.5)
ggplot(measures, aes(x = GlobalTransitivity, fill = NetworkDomain)) + geom_histogram(position = "identity")
#ggplot(measures, aes(x = Reciprocity, fill = NetworkDomain)) + geom_histogram(position = "identity", alpha = 0.5)


#barplot by group
#boxplot measure on y axis, domain on x axis

mean_all <- aggregate(measures[, 4:14], list(measures$NetworkDomain), mean)
?aggregate
setnames(mean_all, "Group.1", "NetworkDomain")

ggplot(measures, aes(x = GlobalTransitivity)) + geom_histogram() 


ggplot(measures, aes(x= Density, fill = NetworkDomain)) + geom_bar(position="fill", stat="identity")


ggplot(measures, aes(x = NetworkDomain, y = AverageTransitivity)) + geom_boxplot()


?degree.distribution
g <- graph_from_data_frame(aids_blogs_2005_, directed = F)

degree.distribution(g)
var(eigen_centrality(g)$vector)

g <- create.igraph.object.OLP(aids_blogs_2005_)
compute.OLP.measures(g, 1)

?geom_density



ggplot(measures, aes(x = GlobalTransitivity, fill = NetworkDomain)) + geom_density()

