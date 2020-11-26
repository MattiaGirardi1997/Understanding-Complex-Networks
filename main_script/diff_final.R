##########################################
## Diffusion Model
## Mattia Girardi
## 18.11.2020
#########################################
list.of.packages <- c("data.table", "igraph", "statnet", "tidyverse", "magrittr", "dplyr")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

### parameters
p.infection <- 1 # probability of infection
pct.starting.infected <- 0.5 # percentage of nodes that start infected
runs <- 10000 # number of iterations

# load in master data
master_data <- fread("output/undirected/master_measures_2.csv")[, c("Name", "NetworkDomain", "number_edges")]

n <- 1
j <- 1
# load in network
file <- as.character(master_data[j, Name])
domain <- as.character(master_data[j, NetworkDomain])
edges <- master_data[j, number_edges]
el <- fread(sprintf("data/all_data/%s.csv", file))
g <- graph_from_data_frame(el, directed = F)

# get number of nodes
n.people <- gorder(g)

# create initial infection information
infected <- sample(
  x = c(T, F),
  size = n.people,
  replace = T,
  prob = c(pct.starting.infected, 1 - pct.starting.infected)
)

# ensure at least one node is infected
if(length(which(infected)) == 0){
  infect <- sample(length(infected), size = 1)
  infected[infect] <- TRUE
}

# ordering nodes
V_order <- sort(as.integer(V(g)$name))

# create data frame of infection indices for each node
infected_data <- data.table(Nodes = V_order, infected = infected)
print(length(which(infected_data$infected))/length(infected_data$infected))
# remove some variables
rm(V_order)

ten.thousands <- 0
t <- 1
while(t < (runs+2)){

  # select random edge
  random.edge <- sample(nrow(el), size = 1)
  el[random.edge]
  c(infected_data[which(el[random.edge, Node1] == infected_data$Nodes), ],
    infected_data[which(el[random.edge, Node2] == infected_data$Nodes), ])

  # determine whether one of the edge's nodes are susceptible
  if (infected_data[which(el[random.edge, Node1] == infected_data$Nodes), infected] !=
      infected_data[which(el[random.edge, Node2] == infected_data$Nodes), infected]){

    # detect susceptible node
    who.susceptible <-  c(el[random.edge, Node1],el[random.edge, Node2])[!c(infected_data[el[random.edge, Node1] == infected_data$Nodes, infected],
                                                                            infected_data[el[random.edge, Node2] == infected_data$Nodes, infected])]

    # detect susceptible node
    infected_data[who.susceptible == Nodes, infected := sample(
      c(T, F),
      size = 1,
      prob = c(p.infection, 1 - p.infection)
    )]
  }
  infected_data[who.susceptible == Nodes]
  print(length(which(infected_data$infected))/length(infected_data$infected))

  # save required number of iterations needed to achieve 70% of nodes infected
  if(length(which(infected_data$infected))/length(infected_data$infected) >= 0.7){
    if(j == 1){
      write.table(data.table(Name = file, Domain = domain, Nodes = n.people, Edges = edges,
                             Iterations_1 = (t + (ten.thousands*runs))),
                  file = sprintf("output/diffusion/diffusion_results_%s_v.2.0.csv", n), sep = ",", row.names = F,
                  col.names = T)
    } else {
      write.table(data.table(file, domain, n.people, edges, (t + (ten.thousands*runs))),
                  file = sprintf("output/diffusion/diffusion_results_%s_v.2.0.csv", n), sep = ",", row.names = F,
                  append = T, col.names = F)
    }
    break
  } else if(t == runs + 1){
    ten.thousands <- ten.thousands + 1
    t <- 1
  }
  t <- t + 1
}






