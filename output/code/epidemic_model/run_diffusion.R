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
# run diffusion
source("R/diffusion_function.R")
master_data <- fread("removed_loops/output/master_measures_removed_loops.csv")
set.seed(1234)
for(n in 1:10){
  for(j in 1:nrow(master_data)){
    simulate.diffusion(j = j, pct.starting.infected = 0.001, p.infection = 1, n = n,
                                     threshold = 0.5)
  }
}

