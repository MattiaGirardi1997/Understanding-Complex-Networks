#########################################
## Modify networks
## Mattia Girardi
## 15.01.2021
########################################

#' Modify networks
#'
#' @param files list of network csv files
#' @return removed loos, simplified and largest component of network as csv file in path
#' @export
#' @import data.table, igraph, dplyr
modify.networks <- function(files, path = "data/final_data",
                            targetfolder = "data/final_data"){
  for(i in 1:length(files)){
    # save network name
    name <- files[i]

    # remove suffix
    name <- gsub(".csv", "", name)

    # skip network_specs.csv file
    if(name == "network_specs"){
      break
    }

    # download edgelist
    el <- fread(paste(path, sprintf("/%s.csv", name), sep = "")) %>%

      # create igraph object
      graph_from_data_frame(directed = F) %>%

      # simplify network
      simplify()

    # decompose network to largest component
    el <- data.table(as_edgelist(decompose(el, min.vertices =
                                             max(components(el)$csize))[[1]]))
    # change column names
    names(el) <- c("Node1", "Node2")

    # transform columns into integers
    el$Node1 <- as.integer(el$Node1)
    el$Node2 <- as.integer(el$Node2)

    # save edgelist to folder
    write.table(el, file = paste(targetfolder, sprintf("/%s.csv", name), sep = ""), row.names = F, sep = ",")

    # clear environment
    rm(el, name)
  }
}



