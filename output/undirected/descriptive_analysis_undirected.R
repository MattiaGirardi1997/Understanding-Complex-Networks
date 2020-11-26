##########################################
## Descriptive Analysis
## Mattia Girardi
## 01.11.2020
#########################################

# set working directory
setwd("~/Desktop/Bachelor Thesis/code/bachelor_thesis")

# install packages
list.of.packages <- c("data.table", "corrplot", "ggplot2", "dplyr", "gridExtra")
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

################################################################
# load in complete measures
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
measures <- fread("output/undirected/master_measures_2.csv")

# Average Degree
summary(measures$AverageDegree)
ggplot(measures, aes(x = AverageDegree, fill = NetworkDomain)) + geom_histogram() +
  facet_grid(NetworkDomain ~ . ) + labs(y = "Count")
ggplot(measures[AverageDegree < 80], aes(x = AverageDegree, fill = NetworkDomain)) + geom_histogram()
+
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
ggplot(measures[DegreeDistribution < 0.03], aes(x = DegreeDistribution, fill = NetworkDomain)) + geom_histogram()
+
  facet_grid(NetworkDomain ~ . ) + labs(y = "Count")

# Density
summary(measures$Density)
ggplot(measures, aes(x = Density, fill = NetworkDomain)) + geom_histogram() +
  facet_grid(NetworkDomain ~ . ) + labs(y = "Count")

# Eigenvector Centrality
summary(measures$EigenvectorCentrality)
ggplot(measures, aes(x = EigenvectorCentrality, fill = NetworkDomain)) + geom_histogram() +
  facet_grid(NetworkDomain ~ . ) + labs(y = "Count")

# Global Transitivity
summary(measures$GlobalTransitivity)
ggplot(measures, aes(x = GlobalTransitivity, fill = NetworkDomain)) + geom_histogram() +
  facet_grid(NetworkDomain ~ . ) + labs(y = "Count")


## comparing means
mean_all <- aggregate(measures[, 5:13], list(measures$NetworkDomain), mean)
setnames(mean_all, "Group.1", "NetworkDomain")

data_long <- tidyr::gather(mean_all, key = type_col, value = categories, -c("NetworkDomain", "DegreeAssortativity"))
ggplot(data_long, aes(x = '', y = categories, fill = NetworkDomain)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_wrap(~ type_col, scales = "free_x")


########################################################
## bivariate plots
measures_bi <- fread("output/undirected/master_measures_2.csv")
measures_bi[measures_bi == 0 | measures_bi == 1,] <- NA
measures_bi[1, 1] <- 1

for(i in 1:length(measures_bi$ID)){
  if(measures_bi[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    measures_bi[i, "NetworkDomain"] <- gsub(".*", "Social", measures_bi[i, "NetworkDomain"])
  } else {
    measures_bi[i, "NetworkDomain"] <- gsub(".*", "Non-Social", measures_bi[i, "NetworkDomain"])
  }
}


ggplot(measures_bi, aes(x = measures_bi[NetworkDomain == "Social",
                                  AveragePathLength],
                     y = measures_bi[NetworkDomain == "Non-Social",
                                     AveragePathLength])) + geom_boxplot()


ggplot(measures_bi, aes(x = NetworkDomain, y = AverageDegree)) + geom_boxplot()
ggplot(measures_bi[AveragePathLength < 10], aes(x = NetworkDomain, y = AveragePathLength)) + geom_boxplot()
ggplot(measures_bi, aes(x = NetworkDomain, y = AverageTransitivity)) + geom_boxplot()
ggplot(measures_bi[Closeness < 0.005], aes(x = NetworkDomain, y = Closeness)) + geom_boxplot()
ggplot(measures_bi, aes(x = NetworkDomain, y = DegreeAssortativity)) + geom_boxplot()
ggplot(measures_bi[DegreeDistribution < 0.005], aes(x = NetworkDomain, y = DegreeDistribution)) + geom_boxplot()
ggplot(measures_bi[Density < 0.08], aes(x = NetworkDomain, y = Density)) + geom_boxplot()
ggplot(measures_bi[EigenvectorCentrality < 0.05], aes(x = NetworkDomain, y = EigenvectorCentrality)) + geom_boxplot()
ggplot(measures_bi[GlobalTransitivity < 0.5], aes(x = NetworkDomain, y = GlobalTransitivity)) + geom_boxplot()


# Average Degree
summary(measures_bi$AverageDegree)
ggplot(measures_bi, aes(x = AverageDegree, fill = NetworkDomain)) + geom_histogram() +
  facet_grid(NetworkDomain ~ . ) + labs(y = "Count")
a <- ggplot(measures_bi[AverageDegree < 20], aes(x = AverageDegree, fill = NetworkDomain)) + geom_histogram() +
  facet_grid(NetworkDomain ~ . ) + labs(y = "Count")
b <- ggplot(measures_bi[AverageDegree < 20], aes(x = NetworkDomain, y = AverageDegree)) + geom_boxplot()
grid.arrange(a, b)


# Average Path Length
summary(measures_bi$AveragePathLength)
a <- ggplot(measures_bi[AveragePathLength < 10], aes(x = AveragePathLength, fill = NetworkDomain)) + geom_histogram() +
  facet_grid(NetworkDomain ~ . ) + labs(y = "Count")
b <- ggplot(measures_bi[AveragePathLength < 10], aes(x = NetworkDomain, y = AveragePathLength)) + geom_boxplot()
grid.arrange(a, b)

# Average Transitivity
summary(measures_bi$AverageTransitivity)
a <- ggplot(measures_bi, aes(x = AverageTransitivity, fill = NetworkDomain)) + geom_histogram() +
  facet_grid(NetworkDomain ~ . ) + labs(y = "Count")
b <- ggplot(measures_bi, aes(x = NetworkDomain, y = AverageTransitivity)) + geom_boxplot()
grid.arrange(a, b)

# Closeness
summary(measures_bi$Closeness)
a <- ggplot(measures_bi[Closeness < 0.005], aes(x = Closeness, fill = NetworkDomain)) + geom_histogram() +
  facet_grid(NetworkDomain ~ . ) + labs(y = "Count")
b <- ggplot(measures_bi[Closeness < 0.005], aes(x = NetworkDomain, y = Closeness)) + geom_boxplot()
grid.arrange(a, b)

# Degree Assortativity
summary(measures_bi$DegreeAssortativity)
a <- ggplot(measures_bi, aes(x = DegreeAssortativity, fill = NetworkDomain)) + geom_histogram() +
  facet_grid(NetworkDomain ~ . ) + labs(y = "Count")
b <- ggplot(measures_bi, aes(x = NetworkDomain, y = DegreeAssortativity)) + geom_boxplot()
grid.arrange(a, b)

# Degree Distribution
summary(measures_bi$DegreeDistribution)
a <- ggplot(measures_bi[DegreeDistribution < 0.005], aes(x = DegreeDistribution, fill = NetworkDomain)) + geom_histogram() +
  facet_grid(NetworkDomain ~ . ) + labs(y = "Count")
b <- ggplot(measures_bi[DegreeDistribution < 0.005], aes(x = NetworkDomain, y = DegreeDistribution)) + geom_boxplot()
grid.arrange(a, b)

# Density
summary(measures_bi$Density)
a <- ggplot(measures_bi[Density < 0.08], aes(x = Density, fill = NetworkDomain)) + geom_histogram() +
  facet_grid(NetworkDomain ~ . ) + labs(y = "Count")
b <- ggplot(measures_bi[Density < 0.08], aes(x = NetworkDomain, y = Density)) + geom_boxplot()
grid.arrange(a, b)

# Eigenvector Centrality
summary(measures_bi$EigenvectorCentrality)
a <- ggplot(measures_bi[EigenvectorCentrality < 0.1], aes(x = EigenvectorCentrality, fill = NetworkDomain)) + geom_histogram() +
  facet_grid(NetworkDomain ~ . ) + labs(y = "Count")
b <- ggplot(measures_bi[EigenvectorCentrality < 0.1], aes(x = NetworkDomain, y = EigenvectorCentrality)) + geom_boxplot()
grid.arrange(a, b)

# Global Transitivity
summary(measures_bi$GlobalTransitivity)
a <- ggplot(measures_bi, aes(x = GlobalTransitivity, fill = NetworkDomain)) + geom_histogram() +
  facet_grid(NetworkDomain ~ . ) + labs(y = "Count")
b <- ggplot(measures_bi, aes(x = NetworkDomain, y = GlobalTransitivity)) + geom_boxplot()
grid.arrange(a, b)



?geom_boxplot

names(measures_bi)
ggplot(measures_bi, aes(x = number_edges, y = AveragePathLength, fill = NetworkDomain)) + geom_boxplot()
+ facet_grid(NetworkDomain ~ . ) + labs(y = "Count")


  facet_grid(NetworkDomain ~ . ) + labs(y = "Count")


length(measures_bi[NetworkDomain == "Social", AveragePathLength])
length(measures_bi[NetworkDomain == "Non-Social", ID][c(1:222)])







attach(measures)
par(mfrow=c(2,2))
a <- ggplot(measures, aes(x = number_edges, y = AverageTransitivity)) + geom_point()
b <- ggplot(measures, aes(x = number_edges, y = AveragePathLength)) + geom_point()
c <- ggplot(measures, aes(x = number_edges, y = GlobalTransitivity)) + geom_point()
d <- ggplot(measures, aes(x = number_edges, y = Closeness)) + geom_point()

grid.arrange(a, b, c, d, nrow = 2, ncol = 2)



*frequency
