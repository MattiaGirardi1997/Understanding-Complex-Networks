#########################################
## Network measure function
## Mattia Girardi
## 04.09.2020
########################################

#' Convert .csv object to igraph object
#'
#' @param network Network name as a .csv object (corresponds to network_name)
#' @return igraph object
#' @export
#' @import igraph
create.igraph.object <- function(network){
  graph_from_data_frame(network[, 1:2], directed = FALSE)
}

#' Compute several network measures of a complex network.
#'
#' @param igraph.network Network as an igraph object.
#' @param i Network index
#' @return ID, Network Name, Reciprocity, Degree Distribution, Transitivity, Degree Assortativity, Betweenness, Closeness,
#' Average Path Lenght, Hierarchy, Density
#' @export
#' @import data.table, DescTools, igraph
compute.measures <- function(igraph.network, i, data = fread("removed_loops/output/removed_loops_table.csv")){
  if(length(E(igraph.network)) == 0){
    network_name <- data[i, Name]
    measures <- data.frame(ID = i, Name = network_name, Nodes = NA, Edges = NA,
                           NetworkDomain = NA, AverageDegree = NA,
                           AveragePathLength = NA, AverageTransitivity = NA,
                           BetweennessCentrality = NA, Closeness = NA,
                           ClosenessCentrality = NA, DegreeAssortativity = NA,
                           DegreeCentrality = NA, DegreeDistribution = NA,
                           Density = NA, EigenvectorCentrality = NA,
                           EigenvectorCentrality_2 = NA, GlobalTransitivity = NA)
  } else {
    ID <- i
    num_edges <- length(E(igraph.network))
    nodes <- length(V(igraph.network))
    network_name <- data[i, Name]
    domain <- data[i, NetworkDomain]

    # other measures
    avg_path_length <- average.path.length(igraph.network)
    if(avg_path_length == 0| is.na(avg_path_length)){
      avg_path_length <- NA
    }
    degree_assortativity <- assortativity.degree(igraph.network)
    if(is.null(degree_assortativity) | is.na(degree_assortativity)){
      degree_assortativity <- NA
    }
    edge_dens <- edge_density(igraph.network)
    if(edge_dens == 0 | is.na(edge_dens)){
      edge_dens <- NA
    }

    # tranistivity
    trnstvty_average <- transitivity(igraph.network, type = "average")
    if(trnstvty_average == 0 | is.na(trnstvty_average)){
      trnstvty_average <- NA
    }
    trnstvty_global <- transitivity(igraph.network, type = "global")
    if(trnstvty_global == 0 | is.na(trnstvty_global)){
      trnstvty_global <- NA
    }
    giniTransitivity <- Gini(transitivity(igraph.network, type = "local"), na.rm = T)
    if(giniTransitivity == 0 | is.na(giniTransitivity)){
      giniTransitivity <- NA
    }

    # degree
    mean_degree <- mean(degree(igraph.network))
    if(mean_degree == 0 | is.na(mean_degree)){
      mean_degree <- NA
    }
    median_degree <- median(degree(igraph.network))
    if(median_degree == 0 | is.na(median_degree)){
      median_degree <- NA
    }

    # centrality measures
    betw_cent <- centr_betw(igraph.network)$centralization
    if(is.na(betw_cent)){
      betw_cent <- NA
    }
    clo_cent <- centr_clo(igraph.network)$centralization
    if(clo_cent == 0| is.na(clo_cent)){
      clo_cent <- NA
    }
    deg_cent <- centr_degree(igraph.network)$centralization
    if(deg_cent == 0 | is.na(deg_cent)){
      deg_cent <- NA
    }
    eigen_cent <- centr_eigen(igraph.network)$centralization
    if(eigen_cent == 0 | is.na(eigen_cent)){
      eigen_cent <- NA
    }

    # Gini coefficient
    giniBetweenness <- Gini(centr_betw(igraph.network)$res)
    if(giniBetweenness == 0 | is.na(giniBetweenness)){
      giniBetweenness <- NA
    }
    giniCloseness <- Gini(centr_clo(igraph.network)$res)
    if(giniCloseness == 0 | is.na(giniCloseness)){
      giniCloseness <- NA
    }
    giniDegree <- Gini(degree(igraph.network))
    if(giniDegree == 0 | is.na(giniDegree)){
      giniDegree <- NA
    }
    giniEigenvectorCentrality <- Gini(centr_eigen(igraph.network)$vector)
    if(giniEigenvectorCentrality == 0 | is.na(giniEigenvectorCentrality)){
      giniEigenvectorCentrality <- NA
    }

    measures <- data.frame(ID = ID, Name = network_name, NetworkDomain = domain,Nodes = nodes,
                           Edges = num_edges,

                           AveragePathLength = avg_path_length, DegreeAssortativity = degree_assortativity,
                           Density = edge_dens,

                           AverageTransitivity = trnstvty_average, GiniTransitivity = giniTransitivity,
                           GlobalTransitivity = trnstvty_global,

                           AverageDegree = mean_degree, MedianDegree = median_degree,

                           BetweennessCentrality = betw_cent, ClosenessCentrality = clo_cent,
                           DegreeCentrality = deg_cent, EigenvectorCentrality = eigen_cent,

                           GiniBetweenness = giniBetweenness, GiniCloseness = giniCloseness,
                           GiniDegreeDistribution = giniDegree, GiniEigenvectorCentrality = giniEigenvectorCentrality)
  }
}

#' Append computed measures to ouptut data table.
#'
#' @param measures Network measure data table.
#' @param i Network index
#' @param path Location folder of data table.
#' @return data table
#' @export
#' @import data.table
append.measures <- function(measures, i, path = "removed_loops/output/removed_loops_measures.csv"){
  if (i == 1){
    write.table(measures, file = path, sep = ",", col.names = TRUE, row.names = FALSE)
  } else {
    write.table(measures, file = path, sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE)
  }
}

#' Compute and append network measures to data table.
#'
#' @param network Network name as an .rda object (corresponds to var_name in ICON_data)
#' @param i Network index
#' @param path Location folder of data table.
#' @return data table
#' @export
#' @import data.table
network.measures <- function(network, i, path = path){
  igraph.network <- create.igraph.object(network)
  measures <- compute.measures(igraph.network, i)
  append.measures(measures, i, path)
}



