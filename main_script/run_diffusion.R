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
master_data <- fread("output/undirected/master_measures_2.csv")[, c("Name", "NetworkDomain", "number_edges")]
master_data[482]
# run diffusion
set.seed(1234)
for(n in 1:5){
  for(j in 482:499){
    simulate.diffusion(j = j, p.infection = 0.5, pct.starting.infected = 0.05, n = n, threshold = 0.7)
  }
}

n.people <- 2
data.table(as.character(c(n.people, "max")))
f <- data.table(c(paste(c("max",300), c((n.people*100), c(400), c(800)))))
f <- data.table(rbind(c(paste("max", 200)), c(300), c(500), c(800), c(paste("max", 300))))

simulate.diffusion(j = 157, p.infection = 0.5, pct.starting.infected = 0.05, n = n, threshold = 0.7)

nw <- read_csv("data/all_data/celegans_interactomes_LCI.csv")
nodes <- c(nw$Node1, nw$Node2)
sort(unique(nodes))
length(unique(nodes))

#########
#ab 497 noch machen
#########


diffusion_results_1 <- fread("output/diffusion/diffusion_results_1.csv")
diffusion_results_1$Ratio <- diffusion_results_1$Nodes/diffusion_results_1$Iterations_1
diffusion_results_1$Ratio2 <- diffusion_results_1$Iterations_1/diffusion_results_1$Nodes

ggplot(diffusion_results_1, aes(x = Nodes, y = Iterations_1, color = Domain)) + geom_point()
ggplot(diffusion_results_1, aes(x = Iterations_1, fill = Domain)) + geom_histogram()
ggplot(diffusion_results_1[Ratio2 < 20], aes(x = Ratio2, fill = Domain)) + geom_histogram()


diffusion_results_1 <- read_csv("output/diffusion/diffusion_results_1.csv")
diffusion_results_1$ratio <- diffusion_results_1$Iterations_1/diffusion_results_1$Nodes

master_data <- fread("output/undirected/master_measures_2.csv")[, c("Name", "NetworkDomain", "number_edges")]
master_data[152]

source("R/diffusion_function.R")













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









