##########################################
## Descriptive Analysis
## Mattia Girardi
## 01.11.2020
#########################################

# set working directory
setwd("~/Desktop/Bachelor Thesis/code/bachelor_thesis")

# install packages
list.of.packages <- c("data.table", "corrplot", "ggplot2", "dplyr", "janitor")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

# load in measures
ICON_measures <- fread("output/undirected/ICON_measures_2.csv")
netzschleuder_measures <- fread("output/undirected/netzschleuder_measures_2.csv")
OLP_measures <- fread("output/undirected/OLP_measures_2.csv")
added_networks_measures <- fread("output/undirected/added_networks_measures_2.csv")

master_measures_2 <- rbind(ICON_measures, netzschleuder_measures, OLP_measures, added_networks_measures)
master_measures_2 <- master_measures_2[order(master_measures_2$Name)]

# deleting projection networks
measures <- master_measures_2[-c(97:101, 102:103, 188:190, 229, 273, 392, 428, 430:541, 545)]

# arxiv_astroph
# arxiv_condmat
# arxiv_grqc
# arxiv_hepph
# arxiv_hepht
# atlas_of_economic_complexity_export_network
# dutch_corporate_boards_(1976,_1996,_2001)
# freshmen_t0
# genetic_multiplex_HepatitusCVirus
# malaria_genes_combined
# network_coauthors
# norwegian_board_directors
# packet_delays

write.table(measures, file = "output/undirected/master_measures_2.csv", sep = ",", row.names = F)
measures <- fread("output/undirected/master_measures_2.csv")
measures <- na.omit(measures)
#### pie chart of domains
prct <- prop.table(table(measures$NetworkDomain))
colors <- c("Green", "Blue", "Red", "Purple", "Black", "Orange", "Yellow")

pie(prct, col = colors, radius = 1.5)

#### correlation matrix
cor <- cor(measures[, c(3, 5:13)], use = "complete.obs")

corrplot(cor)


















# 
# Average Degree
summary(measures$AverageDegree)
ggplot(measures, aes(x = AverageDegree, fill = NetworkDomain)) + geom_histogram() +
  facet_grid(NetworkDomain ~ . ) + labs(y = "Count")
ggplot(measures[AverageDegree < 20], aes(x = AverageDegree, fill = NetworkDomain)) + geom_histogram() +
  facet_grid(NetworkDomain ~ . ) + labs(y = "Count")

# Average Path Length
summary(measures$AveragePathLength)
ggplot(measures, aes(x = AveragePathLength, fill = NetworkDomain)) + geom_histogram() +
  facet_grid(NetworkDomain ~ . ) + labs(y = "Count")

# Average Transitivity
summary(measures$AverageTransitivity)
ggplot(measures, aes(x = AverageTransitivity, fill = NetworkDomain)) + geom_histogram() +
  facet_grid(NetworkDomain ~ . ) + labs(y = "Count")

# Closeness
summary(measures$Closeness)
ggplot(measures, aes(x = Closeness, fill = NetworkDomain)) + geom_histogram() +
  facet_grid(NetworkDomain ~ . ) + labs(y = "Count")

# Betweenness
#summary(measures$Betweenness)
# ggplot(measures, aes(x = Betweenness, fill = NetworkDomain)) + geom_histogram()
                                                                                     
# Degree Assortativity
summary(measures$DegreeAssortativity)
ggplot(measures, aes(x = DegreeAssortativity, fill = NetworkDomain)) + geom_histogram() +
  facet_grid(NetworkDomain ~ . ) + labs(y = "Count")

# Degree Distribution
summary(measures$DegreeDistribution)
ggplot(measures, aes(x = DegreeDistribution, fill = NetworkDomain)) + geom_histogram() +
  facet_grid(NetworkDomain ~ . ) + labs(y = "Count")

# Density
# summary(measures$Density)
# ggplot(measures, aes(x = Density, fill = NetworkDomain)) + geom_histogram(position = "identity", alpha = 0.5)

# Eigenvector Centrality
summary(measures$EigenvectorCentrality)
ggplot(measures, aes(x = EigenvectorCentrality, fill = NetworkDomain)) + geom_histogram() +
  facet_grid(NetworkDomain ~ . ) + labs(y = "Count")

# Global Transitivity
summary(measures$GlobalTransitivity)
ggplot(measures, aes(x = GlobalTransitivity, fill = NetworkDomain)) + geom_histogram() +
  facet_grid(NetworkDomain ~ . ) + labs(y = "Count")

# Reciprocity
# summary(measures$Reciprocity)
# ggplot(measures, aes(x = Reciprocity, fill = NetworkDomain)) + geom_histogram(position = "identity", alpha = 0.5)


## comparing means
mean_all <- aggregate(measures[, 5:13], list(measures$NetworkDomain), mean)
setnames(mean_all, "Group.1", "NetworkDomain")

data_long <- tidyr::gather(mean_all, key = type_col, value = categories, -c("NetworkDomain", "DegreeAssortativity"))
ggplot(data_long, aes(x = '', y = categories, fill = NetworkDomain)) +
  geom_bar(position = "fill", stat = "identity") + 
  facet_wrap(~ type_col, scales = "free_x")

# degree assortativity
ggplot(mean_all, aes(x = '', y = DegreeAssortativity, fill = NetworkDomain)) + geom_bar(position = "fill",
                                                                                  stat = "identity")


data_long <- tidyr::gather(measures[AveragePathLength < 2], key = type_col, value = categories, -c("ID", "Name", "NetworkDomain",
                                                                            "number_edges"))
ggplot(data_long, aes(x = number_edges, y = categories)) +
  geom_point() + 
  facet_wrap(~ type_col, scales = "free_x")

attach(measures)
par(mfrow=c(2,2))
a <- ggplot(measures, aes(x = number_edges, y = AverageTransitivity)) + geom_point()
b <- ggplot(measures, aes(x = number_edges, y = AveragePathLength)) + geom_point()
c <- ggplot(measures, aes(x = number_edges, y = GlobalTransitivity)) + geom_point()
d <- ggplot(measures, aes(x = number_edges, y = Closeness)) + geom_point()

grid.arrange(a, b, c, d, nrow = 2, ncol = 2)




