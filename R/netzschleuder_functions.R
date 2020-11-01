#########################################
## Netzschleuder functions
## Mattia Girardi
## 04.09.2020
########################################

#' Convert .csv object to igraph object
#'
#' @param network Network name as a .csv object (corresponds to network_name)
#' @return igraph object
#' @export
#' @import igraph
create.igraph.object.NS <- function(network, i, netzschleuder_data = fread("input/import_datasets/netzschleuder_essentials.csv")){
  if(netzschleuder_data[i, directed] == TRUE){
    graph_from_data_frame(network[, 1:2], directed = TRUE)
  } else {
    graph_from_data_frame(network[, 1:2], directed = FALSE)
  }
}

#' Compute several network measures of a complex network.
#'
#' @param igraph.network Network as an igraph object.
#' @param i Network index
#' @return ID, Network Name, Reciprocity, Degree Distribution, Transitivity, Degree Assortativity, Betweenness, Closeness,
#' Average Path Lenght, Hierarchy, Density
#' @export
#' @import data.table
compute.NS.measures <- function(igraph.network, i, netzschleuder_data = fread("input/import_datasets/netzschleuder_essentials.csv"),
                                directed = netzschleuder_data[i, directed]){
  ID <- i
  network_name <- as.character(netzschleuder_data[i, network_name])
  domain <- netzschleuder_data[i, networkDomain]
  mean_degree <- mean(degree(igraph.network))
  if(mean_degree == 0){
    mean_degree <- NA
  }
  avg_path_length <- average.path.length(igraph.network)
  if(avg_path_length == 0){
    avg_path_length <- NA
  }
  trnstvty_average <- transitivity(igraph.network, type = "average")
  if(trnstvty_average == 0){
    trnstvty_average <- NA
  }
  btwnness <- var(edge_betweenness(igraph.network, directed = directed))
  if(btwnness == 0){
    btwnness <- NA
  }
  clsness <- var(closeness(igraph.network, normalized = T))
  if(clsness == 0){
    clsness <- NA
  }
  degree_assortativity <- assortativity.degree(igraph.network)
  if(degree_assortativity == 0){
    degree_assortativity <- NA
  }
  degree_distr <- var(degree_distribution(igraph.network))
  if(degree_distr == 0){
    degree_distr <- NA
  }
  edge_dens <- edge_density(igraph.network)
  if(edge_dens == 0){
    edge_dens <- NA
  }
  eigenv <- var(eigen_centrality(igraph.network, directed = directed)$vector)
  if(eigenv == 0){
    eigenv <- NA
  }
  trnstvty_global <- transitivity(igraph.network, type = "global")
  if(trnstvty_global == 0){
    trnstvty_global <- NA
  }
  recipr <- reciprocity(igraph.network)
  measures <- data.frame(ID = ID, Name = network_name, NetworkDomain = domain, AverageDegree = mean_degree,
                         AveragePathLength = avg_path_length, AverageTransitivity = trnstvty_average, 
                         Betweenness = btwnness, Closeness = clsness, DegreeAssortativity = degree_assortativity,
                         DegreeDistribution = degree_distr, Density = edge_dens, EigenvectorCentrality = eigenv,
                         GlobalTransitivity = trnstvty_global, Reciprocity = recipr)
  return(measures)
}

#' Append computed measures to ouptut data table.
#'
#' @param measures Network measure data table.
#' @param i Network index
#' @param path Location folder of data table.
#' @return data table
#' @export
#' @import data.table
append.NS.measures <- function(measures, i, path = "output/netzschleuder_measures.csv"){
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
NS.network.measures <- function(network, i, path = "output/netzschleuder_measures.csv"){
  igraph.network <- create.igraph.object.NS(network, i)
  measures <- compute.NS.measures(igraph.network, i)
  append.NS.measures(measures, i, path)
}



