##########################################
## Running diffusion
## Mattia Girardi
## 28.12.2020
#########################################

# set working directory
setwd("~/Desktop/Bachelor Thesis/code/bachelor_thesis")

# install packages
list.of.packages <- c("data.table", "igraph", "dplyr")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

#######
# run diffusion
source("R/removed_loops_diffusion_function.R")
master_data <- fread("removed_loops/output/master_measures_removed_loops.csv")
set.seed(1234)
for(n in 1:10){
  for(j in 1:nrow(master_data)){
    simulate.removed.loops.diffusion(j = j, pct.starting.infected = 0.01, p.infection = 1, n = n,
                                     threshold = 1)
  }
}

# 0.1-100-50 done
# 1-100-70 done
# 1-100-50 done
# 1-50-70 done
# 1-50-50 done
# 5-100-70 done
# 5-50-70 done

### rerun aborted diffusion with connected networks
# load in measures data
data <- fread("removed_loops/output/master_measures_removed_loops.csv")[-c(103:111)]

for(i in 1:nrow(data)){
  name <- data[i, Name]
  el <- fread(sprintf("data/all_data/%s.csv", name)) %>%
    graph_from_data_frame(directed = F) %>%
    simplify()
  if(i == 1){
    res <- data.table(data[i, 2:4], Connected = is.connected(el))
  } else {
    res <- rbind(res, data.table(data[i, 2:4], Connected = is.connected(el)))
  }
  rm(el, name)
}

run <- res[Connected == FALSE, Name]
run <- which(data$Name %in% run)


# run diffusion
source("R/diffusion_function.R")
set.seed(1234)
for(n in 1:10){
  for(j in run){
    simulate.diffusion(j = j, pct.starting.infected = 0.05, p.infection = 0.5, n = n,
                                     threshold = 0.7)
  }
}

# 0.1-100-50 done
# 1-100-70 done
# 1-100-50 done
# 1-50-70 done
# 1-50-50 done





