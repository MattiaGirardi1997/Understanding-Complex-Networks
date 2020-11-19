##########################################
## netdiffuseR diffusion model
## Mattia Girardi
## 01.11.2020
#########################################

# set working directory
setwd("~/Desktop/Bachelor Thesis/code/bachelor_thesis")

# install packages
list.of.packages <- c("data.table", "igraph", "statnet", "tidyverse", "magrittr", "ggnetwork", "dplyr")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

nw <- fread("data/netzschleuder_data/dnc.csv")
g <- graph_from_data_frame(nw, directed = F)
gorder(g)

n.people <- gorder(g)
p.infection <- 0.5
pct.starting.infected <- 0.01
max.time <- 100
contact.rate <- 0.05 / 2  # prob. of contact between any 2 people in the network


infected <- sample(
  x = c(T, F),      # people can be infected (T) or susceptible (F)
  size = n.people,  # create a vector that is n.people long
  replace = T,      # with replacement, so that >1 person can be infected
  prob = c(pct.starting.infected, 1 - pct.starting.infected)
)

infected

el <- mutate(nw, from.infected = infected[Node1], to.infected = infected[Node2],
       discordant = (from.infected != to.infected))
el


random.edge <- sample(nrow(el), size = 1)
el[random.edge, "discordant"]  # it's not, so we do nothing.

discordant.edge <- sample(which(el$discordant), size = 1)
el[discordant.edge, ] 

who.susceptible <- with(
  el[discordant.edge, ],
  c(Node1, Node2)[!c(from.infected, to.infected)]
)

infected[who.susceptible] <- sample(c(T, F), size = 1, prob = c(p.infection, 1 - p.infection))

 
infections <- vector(length = max.time, mode = "list")

nw <- fread("data/netzschleuder_data/dnc.csv")
g <- graph_from_data_frame(nw, directed = F)

n.people <- gorder(g)
p.infection <- 0.5
pct.starting.infected <- 0.01
max.time <- 3
contact.rate <- 0.05 / 2

infections <- vector(length = max.time, mode = "list")

# Save what we already did as the first iteration
infections[[1]] <- infected

# Quick aside -- what did we create?
head(infections)

set.seed(27708)
for (t in 2:max.time) {
  
  infections[[t]] <- infections[[t - 1]]
  random.edge <- sample(nrow(el), size = 1) # Pick an edge at random
  
  # If the edge is discordant, flip a coin Node2 decide if the infection spreads
  if (with(el[random.edge, ], 
           infections[[t]][Node1] != infections[[t]][Node2])) {
    
    who.susceptible <- with(
      el[random.edge, ],
      c(Node1, Node2)[!c(infections[[t]][Node1], infections[[t]][Node2])]
    )
    
    infected <- sample(
      x = c(T, F),      # people can be infected (T) or susceptible (F)
      size = n.people,  # create a vecNode2r that is n.people long
      replace = T,      # with replacement, so that >1 person can be infected
      prob = c(pct.starting.infected, 1 - pct.starting.infected)
    )
  }
}


infections.by.time <- data.frame(t = 1:max.time, n.infected = sapply(infections, sum))





