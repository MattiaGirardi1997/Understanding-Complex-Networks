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
source("R/removed_loops_diffusion_function.R")
master_data <- fread("removed_loops/output/master_measures_removed_loops.csv")
set.seed(1234)
for(n in 1:10){
  for(j in 1:513){
    simulate.removed.loops.diffusion(j = j, pct.starting.infected = 0.01, p.infection = 1, n = n, threshold = 0.7)
  }
}

n <- 1
simulate.removed.loops.diffusion(j = 128, p.infection = 1, pct.starting.infected = 0.1, n = n, threshold = 0.7)


# 1-100-70
# 1-100-50 done
# 1-50-70
# 1-50-50 done
# 5-50-70
# 5-50-50
# 10-100-70
# 10-100-50









