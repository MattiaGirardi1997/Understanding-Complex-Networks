#########################################
## Index of Colorado Network functions
## Mattia Girardi
## 04.09.2020
########################################

#' Convert .rda object to igraph object
#'
#' @param network Network name as .csv object (corresponds to var_name in ICON_data)
#' @return igraph object
#' @export
#' @import igraph
create.igraph.object.ICON <- function(network, i, ICON_data = fread("input/import_datasets/ICON_data.csv")){
  graph_from_data_frame(network[, 1:2], directed = FALSE)
}

#' Compute several network measures of a complex network.
#'
#' @param igraph.network Network as an igraph object.
#' @param i Network index
#' @return ID, Network Name, Reciprocity, Degree Distribution, Transitivity, Degree Assortativity, Betweenness, Closeness,
#' Average Path Lenght, Hierarchy, Density
#' @export
#' @import data.table
compute.ICON.measures <- function(igraph.network, i, ICON_data = fread("input/import_datasets/ICON_data.csv")){
  ID <- i
  num_edges <- ICON_data[i, number_edges]
  network_name <- as.character(ICON_data[i, network_name])
  domain <- as.character(ICON_data[i, networkDomain])
  mean_degree <- mean(degree(igraph.network, normalized = T))
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
  eigenv <- var(eigen_centrality(igraph.network)$vector)
  if(eigenv == 0){
    eigenv <- NA
  }
  trnstvty_global <- transitivity(igraph.network, type = "global")
  if(trnstvty_global == 0){
    trnstvty_global <- NA
  }
  measures <- data.frame(ID = ID, Name = network_name, number_edges = num_edges, NetworkDomain = domain, AverageDegree = mean_degree,
                         AveragePathLength = avg_path_length, AverageTransitivity = trnstvty_average, 
                         Closeness = clsness, DegreeAssortativity = degree_assortativity,
                         DegreeDistribution = degree_distr, Density = edge_dens, EigenvectorCentrality = eigenv,
                         GlobalTransitivity = trnstvty_global)
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
append.ICON.measures <- function(measures, i, path = "output/ICON_measures.csv"){
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
ICON.network.measures <- function(network, i, path = "output/ICON_measures.csv"){
    igraph.network <- create.igraph.object.ICON(network, i)
    measures <- compute.ICON.measures(igraph.network, i)
    append.ICON.measures(measures, i, path)
}



