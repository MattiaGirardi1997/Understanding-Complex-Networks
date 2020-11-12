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
f <- graph_from_data_frame(fread("data/netzschleuder_data/dnc.csv"), directed = F)


formation <- ~ edges

target.stats <- c(200)

coef.diss <- dissolution_coefs(dissolution = ~ offset(edges), duration = 80)
coef.diss

est1 <- netest(nw, formation, target.stats, coef.diss)


dx <- netdx(est1, nsims = 10, nsteps = 1000)
dx

plot(dx)

init <- init.net(i.num = 50)
param <- param.net(inf.prob = 0.1, act.rate = 5, rec.rate = 0.02)
control <- control.net(type = "SIS", nsteps = 100, nsims = 5)

sim1 <- netsim(est1, param, init, control)
sim1

g <- as.data.frame(sim1)
g[100, "i.num"]


gorder(nw)














