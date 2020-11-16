##########################################
## EPI model
## Mattia Girardi
## 01.11.2020
#########################################

# set working directory
setwd("~/Desktop/Bachelor Thesis/code/bachelor_thesis")

# install packages
list.of.packages <- c("data.table", "network")
install.packages("EpiModel", dependencies = TRUE)
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

# load in data
master_measures <- fread("output/master_measures.csv")
master_measures_2 <- fread("output/undirected/master_measures_2.csv")


set.seed(1234)

nw <- network(fread("data/netzschleuder_data/dnc.csv"))

formation <- ~ edges

target.stats <- c(12085)

coef.diss <- dissolution_coefs(dissolution = ~ offset(edges), duration = 10)
coef.diss

est1 <- netest(nw, coef.diss)
?netest

dx <- netdx(est1, nsims = 2, nsteps = 1000)
dx

# load in Netzschleuder data
netzschleuder_files <- list.files(path = "data/netzschleuder_data", pattern="*.csv", full.names=T)
netzschleuder_essentials <- fread("input/import_datasets/netzschleuder_essentials.csv")

# network simulation parameters
formation <- ~ edges
target.stats <- c()
coef.diss <- dissolution_coefs(dissolution = ~ offset(edges), duration = 80)
# diffusion simulation parameters
init <- init.net(i.num = 1)
param <- param.net(inf.prob = 0.1, act.rate = 5, rec.rate = 0.02)
control <- control.net(type = "SIS", nsteps = 100, nsims = 1)

for(i in 1:lengthnetzschleuder_files){
  name <- as.character(netzschleuder_essentials[i, network_name])
  nw <- fread(sprintf("data/netzschleuder_data/%s.csv", name))
  target.stats <- c(netzschleuder_essentials[i, number_edges])
  est1 <- netest(nw, formation, target.stats, coef.diss)
  sim1 <- netsim(est1, param, init, control)
  simulation <- data.frame(sim1)
  infected <- simulation[100, "i.num"]
}


class(est1)
# 7th_graders; 740 edges; 29 nodes
init <- init.net(i.num = 25)
param <- param.net(inf.prob = 0.01, act.rate = 5, rec.rate = 0.02)
control <- control.net(type = "SIS", nsteps = 100, nsims = 1)

i <- 1
name <- as.character(netzschleuder_essentials[i, network_name])
nw <- fread(sprintf("data/netzschleuder_data/%s.csv", name))
target.stats <- c(netzschleuder_essentials[i, number_edges])
est1 <- netest(nw, formation, target.stats, coef.diss)
sim1 <- netsim(est1, param, init, control)
simulation <- data.frame(sim1)
infected <- simulation[100, "i.num"]
# 25 out of 29 infected

g <- graph_from_data_frame(nw)

# cora; 91500 edges; 23166 nodes
init <- init.net(i.num = 1)
param <- param.net(inf.prob = 0.01, act.rate = 5, rec.rate = 0.2)
control <- control.net(type = "SIS", nsteps = 100, nsims = 1)
i <- 130
name <- as.character(netzschleuder_essentials[i, network_name])
nw <- fread(sprintf("data/netzschleuder_data/%s.csv", name))
target.stats <- c(netzschleuder_essentials[i, number_edges])
est1 <- netest(nw, formation, target.stats, coef.diss)
sim1 <- netsim(est1, param, init, control)
simulation <- data.frame(sim1)
infected <- simulation[100, "i.num"]









