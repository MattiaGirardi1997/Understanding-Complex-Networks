##########################################
## Running diffusion
## Mattia Girardi
## 28.12.2020
#########################################

# set working directory
setwd("~/Desktop/Bachelor Thesis/code/bachelor_thesis")

# install packages
list.of.packages <- c("data.table", "igraph", "dplyr")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

####### 

# run standard diffusion 
source("R/removed_loops_diffusion_function.R")
master_data <- fread("removed_loops/output/removed_loops_measures.csv")
set.seed(1234)
for(n in 1:10){
  for(j in 1:513){
    simulate.removed.loops.diffusion(j = j, p.infection = 1, pct.starting.infected = 0.1, n = n, threshold = 0.7)
  }
}
n <- 1
simulate.removed.loops.diffusion(j = 128, p.infection = 1, pct.starting.infected = 0.1, n = n, threshold = 0.7)
