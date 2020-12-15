#########################################
## Diffusion Model function
## Mattia Girardi
## 04.09.2020
########################################

#' Simulate diffusion on network
#'
#' @param j network index (corresponds to 'Name' column in master_data)
#' @param p.infection probability of infection
#' @param pct.starting.infected percentage of nodes starting infected
#' @param n number of repetitions of diffusion
#' @return number of infection iterations for 70% of nodes infected
#' @export
#' @import data.table, igraph, dplyr
simulate.diffusion <- function(j, p.infection, pct.starting.infected, n, threshold, runs = 10000, master_data =
                                 fread("output/undirected/master_measures_2.csv")[,c("Name", "NetworkDomain", "Edges", "Nodes")]){
  # load in network
  file <- as.character(master_data[j, Name])
  domain <- as.character(master_data[j, NetworkDomain])
  edges <- master_data[j, Edges]
  el <- fread(sprintf("data/all_data/%s.csv", file))

  # get number of nodes
  nodes <- unique(c(el$Node1, el$Node2))
  n.people <- length(nodes)

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
  V_ordered <- sort(as.integer(nodes))

  # create data frame of infection indices for each node
  infected_data <- data.table(Nodes = V_ordered, infected = infected)

  # remove some variables
  rm(V_ordered)

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

    if (infected_data[which(el[random.edge, Node1] == infected_data$Nodes), infected] ==
        infected_data[which(el[random.edge, Node2] == infected_data$Nodes), infected]){
      el <- el[-random.edge]
    }

    print(length(which(infected_data$infected))/length(infected_data$infected))
    print(random.edge)

    # make sure loop does not run indefinitely and print limit of infections
    if((t + (ten.thousands*runs)) > (70*n.people)){
      write.table(data.table(file, domain, n.people, edges, paste("limit:",length(which(infected_data$infected))/length(infected_data$infected))),
                  file = sprintf("output/diffusion/removed/%s%% starting_%s%% prob_%s%% threshold_%s.csv",
                                 (pct.starting.infected*100),(p.infection*100),
                                 (threshold*100), n), sep = ",", row.names = F,
                  append = T, col.names = F)
      break
    }

    # save required number of iterations needed to achieve 70% of nodes infected
    if(length(which(infected_data$infected))/length(infected_data$infected) >= threshold){
      if(j == 1){
        write.table(data.table(Name = file, Domain = domain, Nodes = n.people, Edges = edges,
                               Iterations_1 = (t + (ten.thousands*runs))),
                    file = sprintf("output/diffusion/removed/%s%% starting_%s%% prob_%s%% threshold_%s.csv",
                                   (pct.starting.infected*100),(p.infection*100),
                                   (threshold*100), n), sep = ",", row.names = F)
      } else {
        write.table(data.table(file, domain, n.people, edges, (t + (ten.thousands*runs))),
                    file = sprintf("output/diffusion/removed/%s%% starting_%s%% prob_%s%% threshold_%s.csv",
                                   (pct.starting.infected*100),(p.infection*100),
                                   (threshold*100), n), sep = ",", row.names = F,
                    append = T, col.names = F)
      }
      break
    } else if(t == runs + 1){
      ten.thousands <- ten.thousands + 1
      t <- 1
    }
    t <- t + 1
  }
}



