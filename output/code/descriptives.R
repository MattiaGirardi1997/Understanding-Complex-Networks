######## install packages ########
list.of.packages <- c("data.table", "ggplot2", "gridExtra", "ggpubr")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

### Decriptives
master_data <- fread("output/master_measures.csv")

## transform Social,Offline and Social,Online into logical variables
for(i in 1:nrow(master_data)){
  if(master_data[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master_data[i, "NetworkDomain"] <- gsub(".*", "Social", master_data[i, "NetworkDomain"])
  } else {
    master_data[i, "NetworkDomain"] <- gsub(".*", "Non-Social", master_data[i, "NetworkDomain"])
  }
}

### Descriptives of nodes and edges
ggplot(master_data, aes(x = Nodes, y = Edges, color = NetworkDomain)) +
  geom_point(size = 3) + scale_x_log10() + scale_y_log10(labels = function(x) format(x, scientific = FALSE)) +
  scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Edges*") + xlab("Nodes*") + labs(color = "Network Domain") + theme(panel.background = element_blank(),
                                          panel.grid.major = element_line(colour = "gray85", size = 1),
                                          panel.grid.minor = element_line(colour = "gray85"),
                                          axis.ticks = element_blank(),
                                          axis.title.y = element_text(size = 25),
                                          axis.title.x = element_text(size = 25),
                                          axis.text.x = element_text(size = 20, colour = "Black", angle = 45, hjust = 1),
                                          axis.text.y = element_text(size = 20, colour = "Black", angle = 45, hjust = 1),
                                          legend.key.height = unit(1, "cm"),
                                          legend.key.width = unit(1, "cm"),
                                          legend.position = c(0.8, 0.25),
                                          legend.background = element_rect(size = 0.1, colour = "Black"),
                                          legend.key = element_blank(),
                                          legend.text = element_text(size = 20),
                                          legend.title = element_text(size = 20)) +
  guides(colour = guide_legend(override.aes = list(size=5)))
# + geom_line(data=master_data, aes(x = Nodes, y = ((Nodes-1)*Nodes)/2), color = "black",
 #           size = 0.5, linetype = 5)


##### Scatterplots #####
#### Other Measures
### Average Path Length
a <- ggplot(master_data, aes(x = Nodes, y = AveragePathLength, color = NetworkDomain)) +
  geom_point(size = 0.6) + scale_x_log10() + scale_y_log10() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Average Path Length*") + theme(legend.position = "none",
                                       panel.background = element_blank(),
                                       panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                       panel.grid.minor = element_line(colour = "gray85"),
                                       axis.ticks = element_blank(),
                                       axis.title.y = element_text(size = 7),
                                       axis.title.x = element_blank(),
                                       axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Degree Assortativity
b <- ggplot(master_data, aes(x = Nodes, y = DegreeAssortativity, color = NetworkDomain)) +
  geom_point(size = 0.6) + scale_x_log10() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Degree Assortativity") + ylim(-1,1) + theme(legend.position = "none",
                                                    panel.background = element_blank(),
                                                    panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                    panel.grid.minor = element_line(colour = "gray85"),
                                                    axis.ticks = element_blank(),
                                                    axis.title.y = element_text(size = 7),
                                                    axis.title.x = element_blank(),
                                                    axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))
### Density
c <- ggplot(master_data, aes(x = Nodes, y = Density, color = NetworkDomain)) +
  geom_point(size = 0.6) + scale_x_log10() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Density") + ylim(0,1) + theme(legend.position = "none",
                                      panel.background = element_blank(),
                                      panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                      panel.grid.minor = element_line(colour = "gray85"),
                                      axis.ticks = element_blank(),
                                      axis.title.y = element_text(size = 7),
                                      axis.title.x = element_blank(),
                                      axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Average Degree
d <- ggplot(master_data, aes(x = Nodes, y = AverageDegree, color = NetworkDomain)) +
  geom_point(size = 0.6) + scale_x_log10() + scale_y_log10() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Average Degree*") + theme(legend.position = "none",
                                  panel.background = element_blank(),
                                  panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                  panel.grid.minor = element_line(colour = "gray85"),
                                  axis.ticks = element_blank(),
                                  axis.title.y = element_text(size = 7),
                                  axis.title.x = element_blank(),
                                  axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Median Degree
e <- ggplot(master_data, aes(x = Nodes, y = MedianDegree, color = NetworkDomain)) +
  geom_point(size = 0.6) + scale_x_log10() + scale_y_log10() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Median Degree*") + theme(legend.position = "none",
                                 panel.background = element_blank(),
                                 panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                 panel.grid.minor = element_line(colour = "gray85"),
                                 axis.ticks = element_blank(),
                                 axis.title.y = element_text(size = 7),
                                 axis.title.x = element_blank(),
                                 axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))


### Average Transitivity
f <- ggplot(master_data, aes(x = Nodes, y = AverageTransitivity, color = NetworkDomain)) +
  geom_point(size = 0.6) + scale_x_log10() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Average Transitivity") + ylim(0,1) + theme(legend.position = "none",
                                                   panel.background = element_blank(),
                                                   panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                   panel.grid.minor = element_line(colour = "gray85"),
                                                   axis.ticks = element_blank(),
                                                   axis.title.y = element_text(size = 7),
                                                   axis.title.x = element_blank(),
                                                   axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Global Transivity
g <- ggplot(master_data, aes(x = Nodes, y = GlobalTransitivity, color = NetworkDomain)) +
  geom_point(size = 0.6) + scale_x_log10() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Global Transitivity") + ylim(0,1) + theme(legend.position = "none",
                                                  panel.background = element_blank(),
                                                  panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                  panel.grid.minor = element_line(colour = "gray85"),
                                                  axis.ticks = element_blank(),
                                                  axis.title.y = element_text(size = 7),
                                                  axis.title.x = element_blank(),
                                                  axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Gini Transitivity
h <- ggplot(master_data, aes(x = Nodes, y = GiniTransitivity, color = NetworkDomain)) +
  geom_point(size = 0.6) + scale_x_log10() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Gini Transitivity") + ylim(0,1) + theme(legend.position = "none",
                                                panel.background = element_blank(),
                                                panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                panel.grid.minor = element_line(colour = "gray85"),
                                                axis.ticks = element_blank(),
                                                axis.title.y = element_text(size = 7),
                                                axis.title.x = element_blank(),
                                                axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Betweenness Centrality
i <- ggplot(master_data, aes(x = Nodes, y = BetweennessCentrality, color = NetworkDomain)) +
  geom_point(size = 0.6) + scale_x_log10() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Betweenness Centrality") + ylim(0,1) + theme(legend.position = "none",
                                                     panel.background = element_blank(),
                                                     panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                     panel.grid.minor = element_line(colour = "gray85"),
                                                     axis.ticks = element_blank(),
                                                     axis.title.y = element_text(size = 7),
                                                     axis.title.x = element_blank(),
                                                     axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))
### Closeness Centrality
j <- ggplot(master_data, aes(x = Nodes, y = ClosenessCentrality, color = NetworkDomain)) +
  geom_point(size = 0.6) + scale_x_log10() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Closeness Centrality") + ylim(0,1) + theme(legend.position = "none",
                                                   panel.background = element_blank(),
                                                   panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                   panel.grid.minor = element_line(colour = "gray85"),
                                                   axis.ticks = element_blank(),
                                                   axis.title.y = element_text(size = 7),
                                                   axis.title.x = element_blank(),
                                                   axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Degree Centrality
k <- ggplot(master_data, aes(x = Nodes, y = DegreeCentrality, color = NetworkDomain)) +
  geom_point(size = 0.6) + scale_x_log10() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Degree Centrality") + ylim(0,1) + theme(legend.position = "none",
                                                panel.background = element_blank(),
                                                panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                panel.grid.minor = element_line(colour = "gray85"),
                                                axis.ticks = element_blank(),
                                                axis.title.y = element_text(size = 7),
                                                axis.title.x = element_blank(),
                                                axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Eigenvector Centrality
l <- ggplot(master_data, aes(x = Nodes, y = EigenvectorCentrality, color = NetworkDomain)) +
  geom_point(size = 0.6) + scale_x_log10() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Eigenvector Centrality") + ylim(0,1) + theme(legend.position = "none",
                                                     panel.background = element_blank(),
                                                     panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                     panel.grid.minor = element_line(colour = "gray85"),
                                                     axis.ticks = element_blank(),
                                                     axis.title.y = element_text(size = 7),
                                                     axis.title.x = element_blank(),
                                                     axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Gini Betweenness
m <- ggplot(master_data, aes(x = Nodes, y = GiniBetweenness, color = NetworkDomain)) +
  geom_point(size = 0.6) + scale_x_log10() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Gini Betweenness") + ylim(0,1) + theme(legend.position = "none",
                                               panel.background = element_blank(),
                                               panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                               panel.grid.minor = element_line(colour = "gray85"),
                                               axis.ticks = element_blank(),
                                               axis.title.y = element_text(size = 7),
                                               axis.title.x = element_blank(),
                                               axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Gini Closeness
n <- ggplot(master_data, aes(x = Nodes, y = GiniCloseness, color = NetworkDomain)) +
  geom_point(size = 0.6) + scale_x_log10() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Gini Closeness") + ylim(0,1) + theme(legend.position = "none",
                                             panel.background = element_blank(),
                                             panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                             panel.grid.minor = element_line(colour = "gray85"),
                                             axis.ticks = element_blank(),
                                             axis.title.y = element_text(size = 7),
                                             axis.title.x = element_blank(),
                                             axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Gini Degree Distribution
o <- ggplot(master_data, aes(x = Nodes, y = GiniDegreeDistribution, color = NetworkDomain)) +
  geom_point(size = 0.6) + scale_x_log10() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Gini Degree Distribution") + ylim(0,1) + theme(legend.position = "none",
                                                       panel.background = element_blank(),
                                                       panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                       panel.grid.minor = element_line(colour = "gray85"),
                                                       axis.ticks = element_blank(),
                                                       axis.title.y = element_text(size = 7),
                                                       axis.title.x = element_blank(),
                                                       axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Gini Eigenvector Centrality
p <- ggplot(master_data, aes(x = Nodes, y = GiniEigenvectorCentrality, color = NetworkDomain)) +
  geom_point(size = 0.6) + scale_x_log10() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Gini Eigenvector Centrality") + ylim(0,1) + theme(legend.position = "none",
                                                          panel.background = element_blank(),
                                                          panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                          panel.grid.minor = element_line(colour = "gray85"),
                                                          axis.ticks = element_blank(),
                                                          axis.title.y = element_text(size = 7),
                                                          axis.title.x = element_blank(),
                                                          axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Complexity
q <- ggplot(master_data, aes(x = Nodes, y = Complexity, color = NetworkDomain)) +
  geom_point(size = 0.6) + scale_x_log10() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Complexity") + ylim(0,1) + theme(legend.position = "none",
                                         panel.background = element_blank(),
                                         panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                         panel.grid.minor = element_line(colour = "gray85"),
                                         axis.ticks = element_blank(),
                                         axis.title.y = element_text(size = 7),
                                         axis.title.x = element_blank(),
                                         axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Entropy
r <- ggplot(master_data, aes(x = Nodes, y = Entropy, color = NetworkDomain)) +
  geom_point(size = 0.6) + scale_x_log10() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Entropy") + ylim(0,1) + theme(legend.position = "none",
                                      panel.background = element_blank(),
                                      panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                      panel.grid.minor = element_line(colour = "gray85"),
                                      axis.ticks = element_blank(),
                                      axis.title.y = element_text(size = 7),
                                      axis.title.x = element_blank(),
                                      axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))
#### legend plot
s <- as_ggplot(get_legend(ggplot(master_data, aes(x = NetworkDomain, y = EigenvectorCentrality, color = NetworkDomain)) +
                            geom_point() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
                            ylab("Eigenvector Centrality") + ylim(0,1) + xlab("") + labs(color = "Network Domain") +
                            theme(legend.key.size = unit(1, "cm"),
                                  legend.text = element_text(size = 10),
                                  legend.title = element_text(size = 15))
))

grid.arrange(a, b, c, d, e, f, i, j, k, l, g, m, n, o, p, h, q, r, s, nrow = 4, ncol = 5)

##### Boxplots #####
### Average Path Length
a <- ggplot(master_data, aes(x = NetworkDomain, y = AveragePathLength, color = NetworkDomain)) +
  geom_boxplot() + scale_y_log10() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Average Path Length*") + xlab("") + theme(legend.position = "none",
                                                  panel.background = element_blank(),
                                                  panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                  panel.grid.minor = element_line(colour = "gray85"),
                                                  axis.ticks = element_blank(),
                                                  axis.title.y = element_text(size = 9),
                                                  axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Degree Assortativity
b <- ggplot(master_data, aes(x = NetworkDomain, y = DegreeAssortativity, color = NetworkDomain)) +
  geom_boxplot() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Degree Assortativity") + ylim(-1,1) + xlab("") + theme(legend.position = "none",
                                                               panel.background = element_blank(),
                                                               panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                               panel.grid.minor = element_line(colour = "gray85"),
                                                               axis.ticks = element_blank(),
                                                               axis.title.y = element_text(size = 9),
                                                               axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Density
c <- ggplot(master_data, aes(x = NetworkDomain, y = Density, color = NetworkDomain)) +
  geom_boxplot() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Density") + ylim(0,1) + xlab("") + theme(legend.position = "none",
                                                 panel.background = element_blank(),
                                                 panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                 panel.grid.minor = element_line(colour = "gray85"),
                                                 axis.ticks = element_blank(),
                                                 axis.title.y = element_text(size = 9),
                                                 axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Average Degree
d <- ggplot(master_data, aes(x = NetworkDomain, y = AverageDegree, color = NetworkDomain)) +
  geom_boxplot() + scale_y_log10() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Average Degree*") + xlab("") + theme(legend.position = "none",
                                             panel.background = element_blank(),
                                             panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                             panel.grid.minor = element_line(colour = "gray85"),
                                             axis.ticks = element_blank(),
                                             axis.title.y = element_text(size = 9),
                                             axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Median Degree
e <- ggplot(master_data, aes(x = NetworkDomain, y = MedianDegree, color = NetworkDomain)) +
  geom_boxplot() + scale_y_log10() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Median Degree*") + xlab("") + theme(legend.position = "none",
                                            panel.background = element_blank(),
                                            panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                            panel.grid.minor = element_line(colour = "gray85"),
                                            axis.ticks = element_blank(),
                                            axis.title.y = element_text(size = 9),
                                            axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Global Transivity
f <- ggplot(master_data, aes(x = NetworkDomain, y = GlobalTransitivity, color = NetworkDomain)) +
  geom_boxplot() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Global Transitivity") + ylim(0,1) + xlab("") + theme(legend.position = "none",
                                                             panel.background = element_blank(),
                                                             panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                             panel.grid.minor = element_line(colour = "gray85"),
                                                             axis.ticks = element_blank(),
                                                             axis.title.y = element_text(size = 9),
                                                             axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))
### Gini Transitivity
g <- ggplot(master_data, aes(x = NetworkDomain, y = GiniTransitivity, color = NetworkDomain)) +
  geom_boxplot() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Gini Transitivity") + ylim(0,1) + xlab("") + theme(legend.position = "none",
                                                           panel.background = element_blank(),
                                                           panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                           panel.grid.minor = element_line(colour = "gray85"),
                                                           axis.ticks = element_blank(),
                                                           axis.title.y = element_text(size = 9),
                                                           axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Average Transitivity
h <- ggplot(master_data, aes(x = NetworkDomain, y = AverageTransitivity, color = NetworkDomain)) +
  geom_boxplot() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Average Transitivity") + ylim(0,1) + xlab("") + theme(legend.position = "none",
                                                              panel.background = element_blank(),
                                                              panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                              panel.grid.minor = element_line(colour = "gray85"),
                                                              axis.ticks = element_blank(),
                                                              axis.title.y = element_text(size = 9),
                                                              axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Betweenness Centrality
i <- ggplot(master_data, aes(x = NetworkDomain, y = BetweennessCentrality, color = NetworkDomain)) +
  geom_boxplot() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Betweenness Centrality") + ylim(0,1) + xlab("") + theme(legend.position = "none",
                                                                panel.background = element_blank(),
                                                                panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                                panel.grid.minor = element_line(colour = "gray85"),
                                                                axis.ticks = element_blank(),
                                                                axis.title.y = element_text(size = 9),
                                                                axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Closeness Centrality
j <- ggplot(master_data, aes(x = NetworkDomain, y = ClosenessCentrality, color = NetworkDomain)) +
  geom_boxplot() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Closeness Centrality") + ylim(0,1) + xlab("") + theme(legend.position = "none",
                                                              panel.background = element_blank(),
                                                              panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                              panel.grid.minor = element_line(colour = "gray85"),
                                                              axis.ticks = element_blank(),
                                                              axis.title.y = element_text(size = 9),
                                                              axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))


### Degree Centrality
k <- ggplot(master_data, aes(x = NetworkDomain, y = DegreeCentrality, color = NetworkDomain)) +
  geom_boxplot() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Degree Centrality") + ylim(0,1) + xlab("") + theme(legend.position = "none",
                                                           panel.background = element_blank(),
                                                           panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                           panel.grid.minor = element_line(colour = "gray85"),
                                                           axis.ticks = element_blank(),
                                                           axis.title.y = element_text(size = 9),
                                                           axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Eigenvector Centrality
l <- ggplot(master_data, aes(x = NetworkDomain, y = EigenvectorCentrality, color = NetworkDomain)) +
  geom_boxplot() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Eigenvector Centrality") + ylim(0,1) + xlab("") + theme(legend.position = "none",
                                                                panel.background = element_blank(),
                                                                panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                                panel.grid.minor = element_line(colour = "gray85"),
                                                                axis.ticks = element_blank(),
                                                                axis.title.y = element_text(size = 9),
                                                                axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))
### Gini Betweenness
m <- ggplot(master_data, aes(x = NetworkDomain, y = GiniBetweenness, color = NetworkDomain)) +
  geom_boxplot() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Gini Betweenness") + ylim(0,1) + xlab("") + theme(legend.position = "none",
                                                          panel.background = element_blank(),
                                                          panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                          panel.grid.minor = element_line(colour = "gray85"),
                                                          axis.ticks = element_blank(),
                                                          axis.title.y = element_text(size = 9),
                                                          axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Gini Closeness
n <- ggplot(master_data, aes(x = NetworkDomain, y = GiniCloseness, color = NetworkDomain)) +
  geom_boxplot() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Gini Closeness") + ylim(0,1) + xlab("") + theme(legend.position = "none",
                                                        panel.background = element_blank(),
                                                        panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                        panel.grid.minor = element_line(colour = "gray85"),
                                                        axis.ticks = element_blank(),
                                                        axis.title.y = element_text(size = 9),
                                                        axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))


### Gini Degree Distribution
o <- ggplot(master_data, aes(x = NetworkDomain, y = GiniDegreeDistribution, color = NetworkDomain)) +
  geom_boxplot() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Gini Degree Distribution") + ylim(0,1) + xlab("") + theme(legend.position = "none",
                                                                  panel.background = element_blank(),
                                                                  panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                                  panel.grid.minor = element_line(colour = "gray85"),
                                                                  axis.ticks = element_blank(),
                                                                  axis.title.y = element_text(size = 9),
                                                                  axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Gini Eigenvector Centrality
p <- ggplot(master_data, aes(x = NetworkDomain, y = GiniEigenvectorCentrality, color = NetworkDomain)) +
  geom_boxplot() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Gini Eigenvector Centrality") + ylim(0,1) + xlab("") + theme(legend.position = "none",
                                                                     panel.background = element_blank(),
                                                                     panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                                     panel.grid.minor = element_line(colour = "gray85"),
                                                                     axis.ticks = element_blank(),
                                                                     axis.title.y = element_text(size = 9),
                                                                     axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Complexity
q <- ggplot(master_data, aes(x = NetworkDomain, y = Complexity, color = NetworkDomain)) +
  geom_boxplot() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Complexity") + ylim(0,1) + xlab("") + theme(legend.position = "none",
                                                    panel.background = element_blank(),
                                                    panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                    panel.grid.minor = element_line(colour = "gray85"),
                                                    axis.ticks = element_blank(),
                                                    axis.title.y = element_text(size = 9),
                                                    axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

### Entropy
r <- ggplot(master_data, aes(x = NetworkDomain, y = Entropy, color = NetworkDomain)) +
  geom_boxplot() + scale_color_manual(values = c("gray20", "orangered1", "dodgerblue1")) +
  ylab("Entropy") + ylim(0,1) + xlab("") + theme(legend.position = "none",
                                                 panel.background = element_blank(),
                                                 panel.grid.major = element_line(colour = "gray85", size = 0.5),
                                                 panel.grid.minor = element_line(colour = "gray85"),
                                                 axis.ticks = element_blank(),
                                                 axis.title.y = element_text(size = 9),
                                                 axis.text.x = element_text(size = 6, colour = "Black", angle = 45, hjust = 1))

grid.arrange(a, b, c, d, e, f, i, j, k, l, g, m, n, o, p, h, q, r, nrow = 4, ncol = 5)


