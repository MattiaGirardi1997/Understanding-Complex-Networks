##########################################
## Running diffusion
## Mattia Girardi
## 23.11.2020
#########################################

# set working directory
setwd("~/Desktop/Bachelor Thesis/code/bachelor_thesis")

# install packages
list.of.packages <- c("data.table", "igraph", "statnet", "tidyverse", "magrittr", "dplyr")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

#######
# load in function
source("R/diffusion_function.R")

master_data <- fread("output/undirected/master_measures_2.csv")
master_data[273]

# run standard diffusion
for(j in 520:522){
  simulate.diffusion(j = j, pct.starting.infected = 0.05, p.infection = 0.5, n = n, threshold = 0.7)
}

# running diffusion with randormly generated graphs
source("R/random_diffusion_function.R")
master_data <- fread("output/undirected/master_measures_2.csv")
set.seed(1234)
for(n in 1:10){
  for(j in 1:513){
    simulate.random.diffusion(j = j, p.infection = 1, pct.starting.infected = 0.1, n = n, threshold = 0.7)
  }
}

# running diffusion with removal
source("R/removed_diffusion_function.R")
master_data <- fread("output/undirected/master_measures_2.csv")
set.seed(1234)
for(n in 1:10){
  for(j in 1:513){
    simulate.removed.diffusion(j = j, pct.starting.infected = 0.1, p.infection = 0.5,  n = n, threshold = 0.7)
  }
}










files <- list.files(path = "output/diffusion", pattern="*.csv", full.names=TRUE)[1:60]
for(j in 1:60){
  name <- files[j]
  n <- fread(files[j])
  n$Iterations_1 <- as.integer(n$Iterations_1)
  write.table(n, file = sprintf("%s", name), row.names = F, sep = ",")
}


e <- c()
n <- c()
nodes <- c()
j <- 1
master_data <- fread("output/undirected/master_measures_2.csv")
for(j in 1:531){
  j <- 101
  net <- fread(sprintf("data/all_data/%s.csv", master_data[j, Name]))
  name <- master_data[j, Name]
  node <- unique(c(net$Node1, net$Node2))
  n.people <- length(node)
  Edges <- nrow(net)
  e[j] <- Edges
  n[j] <- name
  nodes[j] <- n.people
  rm(net)
}



