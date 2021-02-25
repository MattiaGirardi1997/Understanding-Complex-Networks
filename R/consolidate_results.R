#########################################
## Consolidation function
## Mattia Girardi
## 15.01.2021
########################################

#' Consolidate diffusion results
#'
#' @param files diffusion results
#' @param path path of diffusion results
#' @param targetfolder target folder of consolidated results
#' @param runs number of simulations per specification
#' @return consolidated diffusion results
#' @export
#' @import data.table, igraph, dplyr
consolidate.results <- function(files, path = "output/diffusion",
                                targetfolder = "output/diffusion/consolidated", runs){
  for(n in 1:(length(files)/runs)){
    for(i in 1:runs){
      if(i == 1){
        # read in first result table
        res_table <- fread(files[i])

        # change name of first iteration column
        names(res_table)[5] <- c("Iteration_1")
      } else {

        # select diffusion results and add numbering
        res <- fread(files[i])[, sprintf("Iteration_%s", i) := Iterations][, 6]

        # bund column to existing result table
        res_table <- cbind(res_table, res)
      }
    }

    # save specification
    spec <- names[1]
    spec <- gsub("_1.csv", "", spec)

    # convert results to integers
    res_table <- data.table(res_table[, 1:4], sapply(res_table[, 5:ncol(res_table)], as.integer))

    # compute mean
    res_table <- res_table[, Mean := rowMeans(res_table[, 5:ncol(res_table)])]

    # save consolidated results
    write.table(res_table, file = sprintf("removed_loops/diffusion/consolidated/%s.csv", name),
                row.names = F, sep = ",")
    names <- names[-c(1:10)]
    files <- files[-c(1:10)]
  }
}
