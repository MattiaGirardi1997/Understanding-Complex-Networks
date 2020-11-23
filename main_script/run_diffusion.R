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

# load in function
source("R/diffusion_function.R")

# run diffusion
set.seed(1234)
for(n in 1:10){
  for(j in 1:200){
    simulate.diffusion(j = j, p.infection = 0.5, pct.starting.infected = 0.05, n = n)
  }
  if(n != 1){
    iter <- fread(sprintf("output/diffusion/diffusion_results_%s.csv", n))[, Iterations]
    master <- cbind(fread("output/diffusion/diffusion_results_1.csv"), iter)
    write.table(master, file = "output/diffusion/diffusion_results_complete.csv")
    }
}


for(n in 1:5){
  if(n == 1){
    res <- fread("output/diffusion/diffusion_results_1.csv")
    write.table(res, file = "output/diffusion/diffusion_results_complete.csv", sep = ",", row.names = F)
    } else {
    iter <- fread(sprintf("output/diffusion/diffusion_results_%s.csv", n))[, sprintf("Iterations_%s", n)
                                                                           := Iterations][, 5]
    master <- cbind(fread("output/diffusion/diffusion_results_complete.csv"), iter)
    write.table(master, file = "output/diffusion/diffusion_results_complete.csv", sep = ",", row.names = F)
  }
}

ggplot(master, aes(x = Nodes, y = means, fill = Domain)) + geom_point()


master <- fread("output/diffusion/diffusion_results_complete.csv")
master
master <- t(master[, 4:8])


data_long <- tidyr::gather(master, key = type_col, value = categories, -c("Domain", "Name", "Nodes"))
ggplot(data_long, aes(x = '', y = categories, col = Name)) +
  geom_point(position = "fill", stat = "identity")









