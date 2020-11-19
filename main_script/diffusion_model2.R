##########################################
## Diffusion Model
## Mattia Girardi
## 18.11.2020
#########################################

# set working directory
setwd("~/Desktop/Bachelor Thesis/code/bachelor_thesis")

# install packages
list.of.packages <- c("data.table", "igraph", "statnet", "tidyverse", "magrittr", "dplyr")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

# set parameters
p.infection <- 0.9 # probability of infection
pct.starting.infected <- 0.6 # percentage of nodes that start infected
max.time <- 30 # number of iterations

# load in master data
master <- fread("output/master_measures.csv")[, Name]

set.seed(1234)
for(j in 18:18){
  # load in network
  file <- as.character(master[j])
  nw <- fread(sprintf("data/all_data/%s.csv", file))
  g <- graph_from_data_frame(nw, directed = F)
  
  # create list to store different iterations
  infections <- vector(length = max.time, mode = "list")
  
  # get number of nodes
  n.people <- gorder(g)
  
  # create infected list
  set.seed(1234)
  infected <- sample(
    x = c(T, F),      
    size = n.people,  
    replace = T,      
    prob = c(pct.starting.infected, 1 - pct.starting.infected)
  )
  
  for(t in 1:30){
    # update infected list
    if(t == 1){
      infections[[t]] <- infected
    } else {
      infections[[t]] <- infections[[t-1]]
    }
    
    # ordering nodes
    V_order <- sort(as.integer(V(g)$name))
    
    # create data frame of infection indices for each node
    infected_data <- data.frame(Nodes = V_order, infected = infections[[t]])
    
    # add infection index to each edge pair
    from.list <- c()
    to.list <- c()
    for(i in 1:length(nw$Node1)){
      node1 <- which(nw[i, Node1] == infected_data$Nodes)
      from.inf <- infected_data[node1, "infected"]
      from.list <- combine(from.list, from.inf)
      node2 <- which(nw[i, Node2] == infected_data$Nodes)
      to.inf <- infected_data[node2, "infected"]
      to.list <- combine(to.list, to.inf)
    }
    
    # create edgelist that shows discordant nodes
    el <- mutate(nw, from.infected = from.list, to.infected = to.list, 
                 discordant = (from.infected != to.infected))
    
    # remove some variables
    rm(node1, from.inf, from.list, node2, to.inf, to.list, V_order)
    
    # select random edge
    random.edge <- sample(nrow(el), size = 1)
    
    # if edge is discordant
    if (el[random.edge, "discordant"] == TRUE){
      
      node1 <- which(el[random.edge, "Node1"] == infected_data[, "Nodes"])
      node2 <- which(el[random.edge, "Node2"] == infected_data[, "Nodes"])
      
      # select susceptible node
      who.susceptible <- c(node1, node2)[!c(infections[[t]][node1], 
                                            infections[[t]][node2])]
      
      # flip coin whether the node will be infected or not
      set.seed(1234)
      infections[[t]][who.susceptible] <- sample(
        c(T, F), 
        size = 1, 
        prob = c(p.infection, 1 - p.infection)
      )
    }
  }
  
  # average number of infections
  res <- mean(sapply(infections, sum))
  
 # save result
  if(j == 1){
    write.table(data.frame(Name = file, Nodes = n.people, Infected = res), file = "output/diffusion/diffusion_results2.csv", sep = ",", row.names = F,
                col.names = T)
  } else {
    write.table(data.frame(file, n.people, res), file = "output/diffusion/diffusion_results2.csv", sep = ",", row.names = F,
                append = T, col.names = F)
  }
}

mean(sapply(infections, sum))

identical(infections[[1]], infections[[20]])

























