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
create.igraph.object.ICON <- function(network){
  if(ICON_data[i, Directed == 1]){
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
compute.ICON.measures <- function(igraph.network, i){
  ID <- i
  network.name <- as.character(ICON_data[i, Var_name])
  domain <- as.character(ICON_data[i, `Network Domain`])
  recipr <- reciprocity(igraph.network)
  if(is.null(recipr)){
    reciprocity <- NA
  }
  degree_distr <- var(degree_distribution(igraph.network))
  if(is.null(degree_distr)){
    degree_distribution <- NA
  }
  trnstvty <- transitivity(igraph.network)
  if(is.null(trnstvty)){
    transitivity <- NA
  }
  degree_assortativity <- assortativity.degree(igraph.network)
  if(is.null(degree_assortativity)){
    degree_assortativity <- NA
  }
  btwnness <- var(betweenness(igraph.network))
  if(is.null(btwnness)){
    betweenness <- NA
  }
  clsness <- var(closeness(igraph.network))
  if(is.null(clsness)){
    closeness <- NA
  }
  avg_path_length <- mean_distance(igraph.network)
  if(is.null(avg_path_length)){
    avg_path_length <- NA
  }
  hrchy <- hierarchy(igraph.network)
  if(is.null(hrchy)){
    hrchy <- NA
  }
  edge_dens <- edge_density(igraph.network)
  if(is.null(edge_dens)){
    edge_dens <- NA
  }
  measures <- data.frame(ID = ID, Name = network.name, networkDomain = domain, Reciprocity = recipr, DegreeDistribution = degree_distr,
                         Transitivity = trnstvty, DegreeAssortativity = degree_assortativity,
                         Betweenness = btwnness, Closeness = clsness,
                         AveragePathLength = avg_path_length, Hierarchy = hrchy,
                         Density = edge_dens)
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
  if(ICON_data[i, Directed == 1]){
    igraph.network <- create.igraph.object.ICON(network)
    measures <- compute.ICON.measures(igraph.network, i)
    append.ICON.measures(measures, i, path)
  } else {
  igraph.network <- create.igraph.object.ICON(network)
  measures <- compute.ICON.measures(igraph.network, i)
  append.ICON.measures(measures, i, path)
  }
}



