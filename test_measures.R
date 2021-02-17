library(data.table)
library(igraph)
library(DescTools)
library(ggplot2)
data <- fread("removed_loops/output/master_measures_removed_loops.csv")
i <- 272
net <- fread(sprintf("removed_loops/data/%s.csv", data[i, Name]))
g <- graph_from_data_frame(net, directed = F)
is.connected(g)
max(components(g)$csize)
is.connected(decompose(g, min.vertices = max(components(g)$csize))[[1]])

deg <- data.table(Degree = 1:length(degree(g)), k = degree(g))
d <- data.table(k = 0:max(degree(g)), dist = degree.distribution(g))
ggplot(d, aes(x = k, y = dist)) + geom_col(width = 3)
ggplot(deg, aes(x = Degree, y = k)) + geom_col(width = 3)

plot(g)

?degree.distribution(g)

edge_density(g, loops = F)
length(E(g))
V(g)

data <- fread("removed_loops/output/master_measures_removed_loops.csv")
for(i in 1:nrow(data)){
  name <- data[i, Name]
  net <- fread(sprintf("removed_loops/data/%s.csv", data[i, Name]))
  net <- graph_from_data_frame(net, directed = F)
  if(i == 1){
    res <- data.table(data[i, 1:4], Connected = is.connected(net), Components = components(net)$no)
  } else {
    res <- rbind(res, data.table(data[i, 1:4], Connected = is.connected(net),
                                 Components = components(net)$no))
  }
}

length(which(res$Connected))
### Gini Coefficients
data <- fread("removed_loops/output/removed_loops_table.csv")
for(i in 1:nrow(data)){
  name <- data[i, Name]
  net <- fread(sprintf("removed_loops/data/%s.csv", data[i, Name]))
  net <- graph_from_data_frame(net, directed = F)
  GiniBetweenness <- Gini(centr_betw(net)$res)
  GiniCloseness <- Gini(centr_clo(net)$res)
  GiniDegreeCount <- Gini(degree(net))
  GiniEigenvectorCentrality <- Gini(centr_eigen(net)$vector)
  GiniTransitivity <- Gini(transitivity(net, type = "local"), na.rm = T)
  if(i == 1){
    write.table(data.table(data[i, 1:4], GiniBetweenness, GiniCloseness,
                           GiniDegreeCount, GiniEigenvectorCentrality,
                           GiniTransitivity), file = "removed_loops/output/gini_coefficients.csv",
                row.names = F, sep = ",")
  } else {
    write.table(data.table(data[i, 1:4], GiniBetweenness, GiniCloseness,
                           GiniDegreeCount, GiniEigenvectorCentrality,
                           GiniTransitivity), file = "removed_loops/output/gini_coefficients.csv",
                row.names = F, sep = ",", col.names = F, append = T)
  }
  rm(net, GiniBetweenness, GiniCloseness,
     GiniDegreeCount, GiniEigenvectorCentrality,
     GiniTransitivity)
}

### Median Degree
data <- fread("removed_loops/output/removed_loops_table.csv")
for(i in 1:nrow(data)){
  name <- data[i, Name]
  net <- fread(sprintf("removed_loops/data/%s.csv", data[i, Name]))
  net <- graph_from_data_frame(net, directed = F)
  MedianDegree <- median(degreee(net))
  if(i == 1){
    write.table(data.table(data[i, 1:4], MedianDegree), file = "removed_loops/output/median_degree.csv",
                row.names = F, sep = ",")
  } else {
    write.table(data.table(data[i, 1:4], MedianDegree), file = "removed_loops/output/median_degree.csv",
                row.names = F, sep = ",", col.names = F, append = T)
  }
  rm(net, MedianDegree)
}






tab <- data[which(!data$Name %in% master_data$Name), c("Name", "NetworkDomain")]
res[res$Name %in% tab$Name]

data$Name %in% master_data$Name










