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
master_data[435]
# run diffusion
set.seed(1234)
for(n in 1:10){
  for(j in 1:534){
    simulate.diffusion(j = j, p.infection = 1, pct.starting.infected = 0.05, n = n, threshold = 0.7)
  }
}

#######
# load in function
source("R/diffusion_function.R")

master_data <- fread("output/undirected/master_measures_2.csv")

for(j in 1:534){
  simulate.diffusion(j = j, p.infection = 0.5, pct.starting.infected = 0.1, n = n, threshold = 0.7)
}
#######
simulate.diffusion(j = 87, p.infection = 0.5, pct.starting.infected = 0.05, n = n, threshold = 0.7)
#######

files <- list.files(path = "output/diffusion", pattern="*.csv", full.names=TRUE)[1:60]
names <- list.files(path = "output/diffusion", pattern="*.csv", full.names=FALSE)[1:60]
files
for(n in 1:6){
  for(i in 1:10){
    if(i == 1){
      res_table <- fread(files[i])
    } else {
      res <- fread(files[i])[, sprintf("Iteration_%s", i) := Iterations_1][, 6]
      res_table <- cbind(res_table, res)
    }
  }
  name <- names[1]
  name <- gsub("_1.csv", "", name)
  res_table <- data.table(res_table[, 1:4], sapply(res_table[, 5:14], as.numeric))
  res_table <- res_table[, Mean := rowMeans(res_table[, 5:14])]
  write.table(res_table, file = sprintf("output/diffusion/consolidated/%s.csv", name), row.names = F,
              sep = ",")
  names <- names[-c(1:10)]
  files <- files[-c(1:10)]
}

res_1 <- fread("output/diffusion/consolidated/results_1.csv")
res_2 <- fread("output/diffusion/consolidated/results_2.csv")
res_3 <- fread("output/diffusion/consolidated/results_3.csv")
res_4 <- fread("output/diffusion/consolidated/results_4.csv")


ggplot(res_1, aes(x = Edges, y = Mean, color = Domain)) + geom_point()
ggplot(res_2, aes(x = Edges, y = Mean, color = Domain)) + geom_point()
ggplot(res_3, aes(x = Edges, y = Mean, color = Domain)) + geom_point()
ggplot(res_4, aes(x = Edges, y = Mean, color = Domain)) + geom_point()


master <- fread("output/undirected/master_measures_2.csv")


glm_fit <- glm(Iterations ~ AverageComplexity + AverageDegree + AveragePathLength + AverageTransitivity +
                BetweennessCentrality + Closeness + ClosenessCentrality + Complexity + DegreeAssortativity +
                DegreeCentrality + DegreeDistribution + Density + EigenvectorCentrality +
                EigenvectorCentrality_2 + Entropy + GlobalTransitivity + Nodes + Edges,
              data = data.table(master, Iterations = res_1$Mean))

summary(glm_fit)



names(master)

files <- list.files(path = "data/all_data", pattern="*.csv", full.names=T)
n <- fread(files[40])
g <- graph_from_data_frame(n, directed = F)


ggplot(master, aes(x = Nodes, y = means, fill = Domain)) + geom_point()

files <- gsub(".csv", "", files)
files[!files %in% master$Name]




data_long <- tidyr::gather(master, key = type_col, value = categories, -c("Domain", "Name", "Nodes"))
ggplot(data_long, aes(x = '', y = categories, col = Name)) +
  geom_point(position = "fill", stat = "identity")



#### running diffusion with randormly generated graphs
source("R/random_diffusion_function.R")

set.seed(1234)
for(n in 1:10){
  for(j in 1:534){
    simulate.diffusion(j = j, p.infection = 1, pct.starting.infected = 0.1, n = n, threshold = 0.7)
  }
}

#### running diffusion with removal
source("R/removed_diffusion_function.R")

set.seed(1234)
for(n in 1:10){
  for(j in 1:534){
    simulate.diffusion(j = j, p.infection = 1, pct.starting.infected = 0.1, n = n, threshold = 0.7)
  }
}


simulate.diffusion(j = 92, p.infection = 1, pct.starting.infected = 0.1, n = n, threshold = 0.7)

master_data <- fread("output/undirected/master_measures_2.csv")
master_data[92]
g <- fread("data/all_data/aminer_citation_network_(2009)_2.csv")

table(master_data$NetworkDomain)

pct.starting.infected <- 1
p.infection <- 1
threshold <- 0.7






files <- list.files(path = "output/diffusion", pattern="*.csv", full.names=TRUE)
for(j in 1:60){
  n <- fread(files[j])
  n <- n[-c(91:93)]
  write.table()
}
