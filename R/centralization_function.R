#########################################
## Centrality functions
## Mattia Girardi
## 04.09.2020
########################################

#' Convert .csv object to igraph object
#'
#' @param network Network name as a .csv object (corresponds to Name in master_measures)
#' @return igraph object
#' @export
#' @import igraph
create.igraph.object.CTR <- function(network, i){
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
compute.CTR.measures <- function(igraph.network, i, data = fread("output/undirected/master_measures_2.csv")[, Name]){
  network_name <- as.character(data[i])
  deg_cent <- centr_degree(igraph.network)$centralization
  if(deg_cent == 0){
    deg_cent <- NA
  }
  betw_cent <- centr_betw(igraph.network)$centralization
  if(is.na(betw_cent) == TRUE){
    betw_cent <- NA
  }
  clo_cent <- centr_clo(igraph.network)$centralization
  if(clo_cent == 0){
    clo_cent <- NA
  }
  eigen_cent <- centr_eigen(igraph.network)$centralization
  if(eigen_cent == 0){
    eigen_cent <- NA
  }
  measures <- data.frame(Name = network_name, DegreeCentrality = deg_cent, BetweennessCentrality = 
                           betw_cent, ClosenessCentrality = clo_cent, EigenvectorCentrality = eigen_cent)
}

#' Append computed measures to ouptut data table.
#'
#' @param measures Network measure data table.
#' @param i Network index
#' @param path Location folder of data table.
#' @return data table
#' @export
#' @import data.table
append.CTR.measures <- function(measures, i, path = "output/undirected/centralization_measures.csv"){
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
CTR.network.measures <- function(network, i, path = "output/undirected/centralization_measures.csv"){
  igraph.network <- create.igraph.object.CTR(network, i)
  measures <- compute.CTR.measures(igraph.network, i)
  append.CTR.measures(measures, i, path)
}



