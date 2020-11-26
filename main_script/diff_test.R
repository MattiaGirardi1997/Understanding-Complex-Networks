list.of.packages <- c("data.table", "igraph", "statnet", "tidyverse", "magrittr", "dplyr")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

### parameters
p.infection <- 1 # probability of infection
pct.starting.infected <- 0.5 # percentage of nodes that start infected
runs <- 5 # number of iterations

# load in master data
master_data <- fread("output/undirected/master_measures_2.csv")[, c("Name", "NetworkDomain")]
master[282]
# set seed for repeatability
set.seed(1234)

j <- 1
# load in network
file <- as.character(master_data[j, Name])
domain <- as.character(master_data[j, NetworkDomain])
el <- fread(sprintf("data/all_data/%s.csv", file))
g <- graph_from_data_frame(el, directed = F)

# create list to store different iterations
infections <- vector(length = 1, mode = "list")

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

# add infection index to each edge pair
from.list <- c()
to.list <- c()

i <- 1
node1 <- infected_data[which(el[i, Node1] == infected_data$Nodes), infected]
from.inf <- infected_data[node1, infected]
from.list <- combine(from.list, from.inf)
node2 <- which(nw[i, Node2] == infected_data$Nodes)
to.inf <- infected_data[node2, infected]
to.list <- combine(to.list, to.inf)

# create edgelist that shows discordant nodes
el <- data.table(Node1 = nw$Node1, Node2 = nw$Node2, from.infected = from.list, to.infected = to.list)


# ordering nodes
V_order <- sort(as.integer(V(g)$name))

# create data frame of infection indices for each node
infected_data <- data.table(Nodes = V_order, infected = infected)

# remove some variables
rm(V_order)

ten.thousands <- 0
t <- 1
  # update infected list
  if(t == 1){
    infections[[t]] <- infected
  } else {
    infections[[t]] <- infections[[t-1]]
  }

  # select random edge
  random.edge <- sample(nrow(el), size = 1)
  el[random.edge]
  infected_data[which(el[random.edge, Node1] == infected_data$Nodes), infected] != infected_data[which(el[random.edge, Node2] == infected_data$Nodes), infected]
  # determine whether one of the edge's nodes are susceptible
  if (infected_data[which(el[random.edge, Node1] == infected_data$Nodes), infected]
      != infected_data[which(el[random.edge, Node2] == infected_data$Nodes), infected]){

    # get Node 1 infection information from infected_data table
    node1 <- which(el[random.edge, Node1] == infected_data$Nodes)
    # get Node 2 infection information from infected_data table
    node2 <- which(el[random.edge, Node2] == infected_data$Nodes)

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

  print(length(which(infected_data$infected))/length(infected_data$infected))

  # save required number of iterations needed to achieve 70% of nodes infected
  if(length(which(infected_data$infected))/length(infected_data$infected) >= 0.7){
    if(j == 1){
      write.table(data.table(Name = file, Domain = domain, Nodes = n.people, Iterations_1 = (length(infections) +
                                                                                               (ten.thousands*runs))),
                  file = sprintf("output/diffusion/diffusion_results_%s2.csv", n), sep = ",", row.names = F,
                  col.names = T)
    } else {
      write.table(data.table(file, domain, n.people, (length(infections) +
                                                        (ten.thousands*runs))),
                  file = sprintf("output/diffusion/diffusion_results_%s2.csv", n), sep = ",", row.names = F,
                  append = T, col.names = F)
    }
    break
  } else if(t == runs + 1){
    ten.thousands <- ten.thousands + 1
    inf <- infections[[t-1]]
    infections <- vector(length = 1, mode = "list")
    infections[[1]] <- inf
    t <- 1
  }
  t <- t + 1
}









g <- fread("output/diffusion/diffusion_results_1.csv")
m <- fread("output/undirected/master_measures_2.csv")

m[Name %in% g$Name]

mm <- data.table(m[Name %in% g$Name], Iterations = g$Iterations,Nodes = g$Nodes)
mm$ratio <- mm$Nodes/mm$Iterations

fit <- lm(data = mm, Iterations ~ AverageDegree + AveragePathLength + AverageTransitivity + Closeness + DegreeAssortativity +
     DegreeDistribution + Density + EigenvectorCentrality + GlobalTransitivity)

summary(fit)

table(mm$NetworkDomain)
ggplot(mm, aes(y = ratio, x = AverageDegree)) + geom_point() + stat_smooth(method = "lm")
ggplot(mm, aes(y = ratio, x = AveragePathLength, color = NetworkDomain)) + geom_point()
ggplot(mm, aes(y = ratio, x = AverageTransitivity, color = NetworkDomain)) + geom_point()
ggplot(mm, aes(y = ratio, x = DegreeAssortativity, color = NetworkDomain)) + geom_point()
ggplot(mm, aes(y = ratio, x = DegreeDistribution, color = NetworkDomain)) + geom_point()
ggplot(mm[Density < 0.5], aes(y = ratio, x = Density, color = NetworkDomain)) + geom_point()
ggplot(mm, aes(y = ratio, x = EigenvectorCentrality, color = NetworkDomain)) + geom_point()
ggplot(mm, aes(y = ratio, x = GlobalTransitivity, color = NetworkDomain)) + geom_point()


ggplot(mm[number_edges < 2000], aes(x = number_edges, y = Iterations, color = NetworkDomain)) + geom_point()
ggplot(mm, aes(x = number_edges, y = Nodes)) + geom_point()



f <- fread("output/undirected/master_measures_2.csv")[number_edges < 150000]

g <- fread("data/all_data/douban.csv")
h <- graph_from_data_frame(g, directed = F)
length(V(h)$name)

ggplot(mm, aes(x = Nodes, y = Iterations, color = NetworkDomain)) + geom_point()

ggplot(g, aes(x = ratio, fill = Domain)) + geom_histogram()



ggplot(g[Iterations < 50000], aes(x = ratio, y = Nodes, color = Domain)) + geom_point()






