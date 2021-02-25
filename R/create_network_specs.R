#########################################
## Create network specs
## Mattia Girardi
## 15.01.2021
########################################

#' Create network specs
#'
#' @param files list of network csv files
#' @return create input data table
#' @export
#' @import data.table, igraph, dplyr
network.specs <- function(files, path = "data/final",
                          targetpath = "input/network_specs.csv",
                          data = fread("data/all_data/network_specs.csv")){
  for(i in 1:length(files)){

    # save network name
    name <- files[i]
    name <-  gsub(".csv", "", name)

    # download network
    net <- fread(paste(path, sprintf("/%s.csv", name), sep = ""))

    # save network domain
    domain <- data[Name == name, NetworkDomain]

    # create igraph object
    net <- graph_from_data_frame(net, directed = F)

      # compute number of nodes & edges
      nodes <- length(V(net))
      edges <- length(E(net))

    # save output
    if(i == 1){
      write.table(data.table(Name = name, NetworkDomain = domain, Nodes = nodes, Edges = edges),
              file = targetpath, sep = ",", col.names = TRUE, row.names = FALSE)
    } else {
      write.table(data.table(Name = name, NetworkDomain = domain, Nodes = nodes, Edges = edges),
              file = targetpath, sep = ",", col.names = FALSE, row.names = FALSE,
              append = TRUE)
    }
    # clear environment
    rm(net)
  }
}
