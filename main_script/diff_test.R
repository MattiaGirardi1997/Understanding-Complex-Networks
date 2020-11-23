list.of.packages <- c("data.table", "igraph", "statnet", "tidyverse", "magrittr", "dplyr")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

### parameters
p.infection <- 1 # probability of infection
pct.starting.infected <- 0.3 # percentage of nodes that start infected
max.time <- 30 # number of iterations

# load in master data
master <- fread("output/undirected/master_measures_2.csv")[, Name]

# set seed for repeatability
set.seed(1234)

j <- 1
# load in network
file <- as.character(master[j])
nw <- fread(sprintf("data/all_data/%s.csv", file))
g <- graph_from_data_frame(nw, directed = F)


n.people <- gorder(g)

infections <- vector(length = 1, mode = "list")

infected <- sample(
  x = c(T, F),      
  size = n.people,  
  replace = T,      
  prob = c(pct.starting.infected, 1 - pct.starting.infected)
)
length(which(infected))


t <- 2
# create infected list
if(t == 1){
  infections[[t]] <- infected
} else {
  infections[[t]] <- infections[[t-1]]
}

V_order <- sort(as.integer(V(g)$name))
infected_data <- data.table(Nodes = V_order, infected = infections[[t]])

from.list <- c()
to.list <- c()

for(i in 1:length(nw$Node1)){
  node1 <- which(nw[i, Node1] == infected_data$Nodes)
  from.inf <- infected_data[node1, infected]
  from.list <- combine(from.list, from.inf)
  node2 <- which(nw[i, Node2] == infected_data$Nodes)
  to.inf <- infected_data[node2, infected]
  to.list <- combine(to.list, to.inf)
}

el <- data.table(Node1 = nw$Node1, Node2 = nw$Node2, from.infected = from.list, to.infected = to.list)

rm(node1, from.inf, from.list, node2, to.inf, to.list)

set.seed(1234)
random.edge <- sample(nrow(el), size = 1)
random.edge <- 101
el[random.edge,]

# determine whether one of the edge's nodes are susceptible
if (el[random.edge, from.infected] != el[random.edge, to.infected]){
  
  # get Node 1 infection information from infected_data table
  node1 <- which(el[random.edge, Node1] == infected_data[, Nodes])
  # get Node 2 infection information from infected_data table
  node2 <- which(el[random.edge, Node2] == infected_data[, Nodes])
  
  # detect susceptible node
  who.susceptible <- c(node1, node2)[!c(infections[[t]][node1], 
                                        infections[[t]][node2])]
  
  # sample whether susceptible node will be infected 
  infections[[t]][who.susceptible] <- sample(
    c(T, F), 
    size = 1, 
    prob = c(p.infection, 1 - p.infection)
  )
  # if susceptible node gets infected ...
  if(infections[[t]][who.susceptible] == TRUE){
    # AND if newly infected node is Node1 ...
    if(which(!el[random.edge, c(from.infected, to.infected)]) == 1){
      # update infection information in el
      el[Node1 == el[random.edge, Node1], from.infected := TRUE]
      el[Node2 == el[random.edge, Node1], to.infected := TRUE]
      # update infection informagtion in infected_data
      infected_data[Nodes == el[random.edge, Node1], infected := TRUE]
    } 
    # AND if newly infected node is Node2
    else if (which(!el[random.edge, c(from.infected, to.infected)]) == 2){
      # update infection information in el
      el[Node1 == el[random.edge, Node2], from.infected := TRUE]
      el[Node2 == el[random.edge, Node2], to.infected := TRUE]
      # update infection informagtion in infected_data
      infected_data[Nodes == el[random.edge, Node2], infected := TRUE]
    }
  }
  
}

if(length(which(infected_data$infected))/length(infected_data$infected) > 0.7){
  if(j == 1){
    write.table(data.table(Name = file, Nodes = n.people, Iterations = length(infections)), file = "output/diffusion/diffusion_results2.csv", sep = ",", row.names = F,
                col.names = T)
  } else {
    write.table(data.frame(file, n.people, res), file = "output/diffusion/diffusion_results2.csv", sep = ",", row.names = F,
                append = T, col.names = F)
  }
  
}



sapply(infections, sum)


?setkey
?subset
setkey(el, Node1, Node2)

subset(el, Node1 == el[random.edge, Node1], from.infected)



el[Node1 == el[random.edge, Node1], from.infected := TRUE]
el[Node1 == el[random.edge, Node1], susceptible := FALSE]

el[Node1 == el[random.edge, Node1], from.infected]


el <- replace(el[1:2, from.infected], "TRUE")

el

i <- 1
for(i in 1:i){
  i <- i + 1
  print(i)
  if(i == 10){
    break
  }
}


