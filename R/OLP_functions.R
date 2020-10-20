##########################################
## Optimal Link Prediction functions
## Mattia Girardi
## 02.10.2020
#########################################

#' Convert OLP network data into edgelist
#'
#' @param OLP.network Optimal Link Prediction network as list
#' @return data.frame
#' @export
#' @import data.table
#' @import tidyr
#' @import BBmisc
convert.OLP <- function(OLP.network){
  OLP_split <- strsplit(OLP.network, split = "[^[:alnum:]]+")
  OLP_cut <- OLP_split[[1]][-1]
  OLP_df <- as.data.frame(chunk(OLP_cut, 2))

  OLP_final <- as.data.frame(t(OLP_df))
  OLP_final <- setNames(OLP_final, c("Node1", "Node2"))
  row.names(OLP_final) <- c(1:length(OLP_final$Node1))

  OLP_final <- transform(OLP_final, Node1 = as.numeric(as.character(Node1)),
                         Node2 = as.numeric(as.character(Node2)))

  OLP_final$Node1 = OLP_final$Node1 + 1
  OLP_final$Node2 = OLP_final$Node2 + 1
  return(OLP_final)
}

#' Convert .csv object to igraph object
#'
#' @param network Network name as a .csv object (corresponds to network_name)
#' @return igraph object
#' @export
#' @import igraph
create.igraph.object.OLP <- function(network){
  graph_from_edgelist(as.matrix(network[, 1:2]))
}

#' Compute several network measures of a complex network.
#'
#' @param igraph.network Network as an igraph object.
#' @param i Network index
#' @return ID, Network Name, Network Domain, Reciprocity, Degree Distribution, Transitivity, Degree Assortativity, Betweenness, Closeness,
#' Average Path Lenght, Hierarchy, Density
#' @export
#' @import data.table
compute.OLP.measures <- function(igraph.network, i, OLP_data =
                                   fread("input/import_datasets/OLP_essentials.csv")
                                                 [, c("network_name", "networkDomain")]){
  ID <- i
  network.name <- as.character(OLP_data[i, "network_name"])
  domain <- as.character(OLP_data[i, "networkDomain"])
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
  measures <- data.frame(ID = ID, Name = network.name, NetworkDomain = domain, Reciprocity = recipr, DegreeDistribution = degree_distr,
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
append.OLP.measures <- function(measures, i, path = "output/OLP_measures.csv"){
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
OLP.network.measures <- function(network, i, path = "output/OLP_measures.csv"){
  igraph.network <- create.igraph.object.OLP(network)
  measures <- compute.OLP.measures(igraph.network, i)
  append.OLP.measures(measures, i, path)
}
