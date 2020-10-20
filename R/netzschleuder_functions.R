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
create.igraph.object.NS <- function(network){
  graph_from_edgelist(as.matrix(network[, 1:2]))
}

#' Compute several network measures of a complex network.
#'
#' @param igraph.network Network as an igraph object.
#' @param i Network index
#' @return ID, Network Name, Reciprocity, Degree Distribution, Transitivity, Degree Assortativity, Betweenness, Closeness,
#' Average Path Lenght, Hierarchy, Density
#' @export
#' @import data.table
compute.NS.measures <- function(igraph.network, i, netzschleuder_data = fread(("input/import_datasets/OLP_data_essentials.csv")
                                                                              [, c("network_name", "networkDomain")])){
  ID <- i
  network.name <- as.character(netzschleuder_data[i, 1])
  domain <- netzschleuder_data[i, 2]
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
  igraph.network <- create.igraph.object.NS(network)
  measures <- compute.NS.measures(igraph.network, i)
  append.NS.measures(measures, i, path)
}



