######## install packages ########
list.of.packages <- c("data.table", "igraph", "jsonlite", "ggplot2", "gridExtra", "mlbench",
                      "caret", "e1071", "corrplot")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)


# load in measures data
diffusion_data <- fread("removed_loops/output/master_diffusion.csv")
data <- fread("removed_loops/output/master_measures_removed_loops.csv")

for(i in 1:nrow(data)){
  name <- data[i, Name]
  net <- fread(sprintf("removed_loops/data/%s.csv", data[i, Name]))
  net <- graph_from_data_frame(net, directed = F)
  if(i == 1){
    res <- data.table(data[i, 2:4], Connected = is.connected(net), Components = components(net)$no,
                      Max = max(components(net)$csize))
  } else {
    res <- rbind(res, data.table(data[i, 2:4], Connected = is.connected(net),
                                 Components = components(net)$no, Max = max(components(net)$csize)))
  }
}
res$diff <- res$Max/res$Nodes
res <- res[Connected == FALSE, Name]
data[which(data$Name %in% res), Name]

run <- which(data$Name %in% res)
# run diffusion
source("R/removed_loops_diffusion_function.R")

set.seed(1234)
for(n in 1:10){
  for(j in run){
    simulate.removed.loops.diffusion(j = j, pct.starting.infected = 0.001, p.infection = 1, n = n,
                                     threshold = 0.5)
  }
}