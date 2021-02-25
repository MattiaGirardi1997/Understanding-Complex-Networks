##############################################
## Mattia Girardi
## Bachelor Thesis
## 24.02.2020
##############################################
## Main Script
## Understanding Complex Social Networks: The
## impact of social features on diffusion
##############################################
## Supervision:
## Dr. Radu Tanase
## Prof. Dr. René Algesheimer
## Chair for Marketing and Market Research
## University of Zurich (UZH)
##

#### Description
# This R script holds the code for our thesis "Understanding Complex Social Networks: The
# impact of social features on diffusion".
#
# We provide the functions used to compute the
# plots in our thesis.
#
# In chapter 9, we introduce our proposed SI diffusion model. The para-
# meters are adjustable to allow further simulations.
#
# The network data was stored on Google drive and was assembled from network repositories.
# Repository referecnes are at the end of the script
#
# The script will begin by creating a folder to set up a working environment. The path and name
# of this folder can be adjusted here...
dir.create("~/Desktop/understanding_complex_networks")

# set working directory
setwd("~/Desktop/understanding_complex_networks")

# install packages
list.of.packages <- c("data.table", "igraph", "jsonlite", "ggplot2", "gridExtra", "mlbench",
                      "caret", "e1071", "corrplot", "googledrive", "purrr")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

# create folders
dir.create("data")
dir.create("R")
dir.create("output")
dir.create("input")

#### 1) Import data fom Google drive ####
### import network data
  # create folder and set directory
  dir.create("data/raw_data")
  setwd("~/Desktop/understanding_complex_networks/data/raw_data")

  # save folder url
  folder_url <- "https://drive.google.com/drive/folders/1SE5yGMXkfwlkgSlXTDVAVrtHdGzKP8cU?usp=sharing"

  ## identify this folder on Drive
  ## let googledrive know this is a file ID or URL, as opposed to file name
  folder <- drive_get(as_id(folder_url))

  ## identify the csv files in that folder
  csv_files <- drive_ls(folder, type = "csv")

  ## download them
  walk(csv_files$id, ~ drive_download(as_id(.x)));setwd("~/Desktop/understanding_complex_networks")

  ### import R functions
  # set directory
  setwd("~/Desktop/understanding_complex_networks/R")

  # save folder url
  folder_url <- "https://drive.google.com/drive/folders/1XonG4zQDdoIXYevZzc-f-HFKANOuRyhS?usp=sharing"

  ## identify this folder on Drive
  ## let googledrive know this is a file ID or URL, as opposed to file name
  folder <- drive_get(as_id(folder_url))

  ## identify the R scripts in that folder
  R_files <- drive_ls(folder, type = ".R")

#### 2) Modify networks (remove loops, simplify, decompose to largest component) ####
# load function
source("R/modify_networks.R")

# create files list
files <- list.files("data/raw", pattern = "*.csv", full.names = F)

# create target folder
dir.create("data/final_data")

# apply function
modify.networks(files, path = "data/raw", targetfolder = "data/final_data")

# create updated network specs
# load function
source("R/create_network_specs.R")

# load input
files <- list.files("data/final_data", pattern = "*.csv", full.names = F)
data <- fread("data/all_data/network_specs.csv")

# apply function
network.specs(files = files, data = data)
#### 3) Compute measures ####
# load function
source("R/measure_function.R")

# load network specs
data <- fread("input/network_specs.csv")

# run measures
for(i in 1:nrow(data)){
  # download network
  net <- fread(sprintf("data/final_data/%s.csv", data[i, Name]))

  # compute and save measures
  network.measures(net, i, path = "output/master_measures.csv")

  # clear environment
  rm(net)
}

## compute entropy and complexity meaures in python with script
  # convert .csv files to .txt to compute in python
  # create folder
  dir.create("data/final_data_txt")

  # load measures table
  master <- fread("output/master_measures.csv")

    for(i in 1:nrow(master)){
      # save network name
      name <- master[i, Name]

      # download network edgelist
      net <- fread(sprintf("data/final_data/%s.csv", name))

      # save .txt file
      write.table(net, file = sprintf("data/final_data_txt/%s.txt", name), row.names = F, sep = " ", col.names = F)
    }

  # compute measures in python with script:
  drive_download("https://drive.google.com/file/d/1nsshHIhlJCLbznq8GeIU6y-PFj1QYOYl/view?usp=sharing",
               path = "R/complexity_and_entropy_function.py")

      # ... and add to master_measures.csv
        master <- fread("output/master_measures.csv")

        # read in measures
        complexity_and_entropy_measures <- fread("output/complexity_and_entropy_measures.csv")[, -1]

        # remove suffix
        complexity_and_entropy_measures$Name <- gsub(".txt", "", complexity_and_entropy_measures$Name)

        # order by name
        complexity_and_entropy_measures <- complexity_and_entropy_measures[order(complexity_and_entropy_measures$Name)]

        # check that data names are identical
        complexity_and_entropy_measures <- complexity_and_entropy_measures[Name %in% measures$Name]


        # include complexity and entropy in master_measures.csv
        master_measures <- data.table(measures[,1:13], Complexity = complexity_and_entropy_measures$Complexity,
                                      Entropy = complexity_and_entropy_measures$Entropy,
                                      measures[,14:21])

        # update master_measures.csv
        write.table(master_measures, file = "output/master_measures.csv", sep = ",", row.names = F)

#### 4) Run diffusion simulation ####
# create folder
dir.create("output/diffusion")

# load function
source("R/diffusion_function.R")

# load data
master <- fread("output/master_measures.csv")

# set seed for repeatability
set.seed(1234)

# run diffusion
  # set percentage of population starting infected
  pct.starting.infected <- NA

  # set probability of infection
  p.infection <- NA

  # set threshold
  threshold <- NA

  # set number simulations per specification
  run <- NA

  # run diffusion
  for(n in 1:run){
    for(i in 1:nrow(master)){
      simulate.diffusion(i = i, pct.starting.infected = pct.starting.infected,
                         p.infection = p.infection,
                         threshold = threshold,
                         n = n)
    }
  }

#### 5) Consolidate diffusion results ####
# create folder
dir.create("output/diffusion/consolidated")

# load diffusion files
files <- list.files(path = "output/diffusion", pattern="*.csv", full.names=TRUE)
  # detect number of simulations per specification
  runs <- length(grep(unlist(lapply(files, function(x) gsub("d_.*", "", x)))[1],
                      files))

# save result name
names <- list.files(path = "output/diffusion", pattern="*.csv", full.names=FALSE)

# load function
source("R/consolidate_results.R")

# apply function
consolidate.results(files, runs, targetfolder = "output/diffusion/consolidated")

### create master_diffusion for further analysis
# load measures
master <- fread("output/master_measures.csv")

# load consolidated files
files <- list.files("output/diffusion/consolidated", pattern = "*.csv", full.names = T)

for(i in 1:length(files)){
  # save spec name
  spec <- paste("D", sub(".*/(.+)% s.*", "\\1", files[i]),
                sub(".*_(.+)% p.*", "\\1", files[i]),
                sub(".*_(.+)% t.*", "\\1", files[i]), sep = "_")

  # read file
  result <- fread(files[i])

  # assign file to spec name
  assign(spec, result)

  #
  master[, sprintf("%s", spec)] <- result$Mean
}

# save table
write.table(master, file = "output/master_diffusion.csv", sep = ",", row.names = F)

#### 6) Create index descriptives ####
# load master measures
master <- fread("output/master_measures.csv")

# transform Social,Offline and Social,Online into logical variables
for(i in 1:nrow(master)){
  if(master[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master[i, "NetworkDomain"] <- gsub(".*", "Social", master[i, "NetworkDomain"])
    } else {
      master[i, "NetworkDomain"] <- gsub(".*", "Non-Social", master[i, "NetworkDomain"])
    }
  }

# load function
source("R/descriptives_function.R")

# plot nodes and edges
plot.nodes.and.edges(measures = master)

# scatterplots
index.scatterplots(measures = master)

# boxplots
index.boxplots(measures = master)

#### 7) Create index corrplot and identify highly correlated indices ####
#### correlation matrix
master <- fread("output/master_measures.csv")

### create corrplot
cor <- cor(master[, 4:ncol(master)], use = "complete.obs")

corrplot(cor, method = "color", tl.col = "black", tl.offset = 0.5,
         cl.align.text = "l", addgrid.col = "black", tl.cex = 0.9)

### investigate correlation
# ensure the results are repeatable
set.seed(1234)
# calculate correlation matrix
correlationMatrix <- cor(master[,4:ncol(master)])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.75)
# print indexes of highly correlated attributes
names(master[,4:ncol(master)])[print(highlyCorrelated)]

#### 8) Machine Learning ####
# 8.1) Classification ####
#### load in master
master <- fread("output/master_measures.csv")

#### transform Social,Offline and Social,Online into factor variables with 2 levels
for(i in 1:nrow(master)){
  if(master[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master[i, "NetworkDomain"] <- gsub(".*", "1", master[i, "NetworkDomain"])
  } else {
    master[i, "NetworkDomain"] <- gsub(".*", "0", master[i, "NetworkDomain"])
  }
}

master$NetworkDomain <- as.factor(master$NetworkDomain)

# load function
source("R/classify_networks.R")

# set seed for repeatability
set.seed(1234)

# parting data into 95% trainig data and 5% test data; repeat 10'000 times
partitions <- createDataPartition(master$NetworkDomain, times = 10000, p=0.95, list=FALSE)

# classification
classify.networks(master, partitions, fit =

                    NetworkDomain ~

                    Nodes + AveragePathLength + DegreeAssortativity +

                    AverageDegree +

                    GiniTransitivity +

                    GiniCloseness + GiniDegreeDistribution + GiniBetweenness)

# 8.2) Feature importance; logistic regression ####
# load measures
master <- fread("output/master_measures.csv")

#### transform Social,Offline and Social,Online into factor variables with 2 levels
for(i in 1:nrow(master)){
  if(master[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master[i, "NetworkDomain"] <- gsub(".*", "1", master[i, "NetworkDomain"])
  } else {
    master[i, "NetworkDomain"] <- gsub(".*", "0", master[i, "NetworkDomain"])
  }
}

master$NetworkDomain <- as.factor(master$NetworkDomain)

# standardization
master[, 4:length(master)] <- data.table(apply(master[, 4:length(master)],
                                                         2, scale))

# load function
source("R/feature_importance_logit.R")
feature.importance.logit(master, fit =

                           NetworkDomain ~

                           Nodes + AveragePathLength + DegreeAssortativity +

                           AverageDegree +

                           GiniTransitivity +

                           GiniCloseness + GiniDegreeDistribution + GiniBetweenness)


#### 9) Epidemic Model ####

# 9.1) Diffusion descriptives ####
# load function
source("R/diffusion_results_function.R")

# load consolidated files
files <- list.files("output/diffusion/consolidated", pattern = "*.csv", full.names = T)

# apply function
plot.diffusion.results(files)

# 9.2) Spread of diffusion results ####
# load function
source("R/diffusion_results_function.R")

# load consolidated files
files <- list.files("output/diffusion/consolidated", pattern = "*.csv", full.names = T)

# apply function
plot.diffusion.spread(files)

# 9.3) Feature importance; diffusion regression ####
# load function
source("R/diffusion_results_function.R")

# load consolidated files
files <- list.files("output/diffusion/consolidated", pattern = "*.csv", full.names = T)

# load in measures data
master_diffusion <- fread("output/master_diffusion.csv")

# standardization
master_diffusion[, 4:length(master_diffusion)] <- data.table(apply(master_diffusion[, 4:length(master_diffusion)],
                                                         2, scale))

# compute feature importance
feature.importance.diffusion(master_diffusion, files, fit =
                               cbind(D_0.1_100_50, D_1_50_50, D_1_100_50, D_1_50_70, D_1_100_70,
                                     D_5_50_70, D_5_100_70) ~
                               Nodes + AveragePathLength + DegreeAssortativity +

                               AverageDegree +

                               GiniTransitivity +

                               GiniCloseness + GiniDegreeDistribution + GiniBetweenness)

# create feature rank corrplot
feature.rank.corrplot(master_diffusion,  fit =
                        cbind(D_0.1_100_50, D_1_50_50, D_1_100_50, D_1_50_70, D_1_100_70,
                              D_5_50_70, D_5_100_70) ~
                        Nodes + AveragePathLength + DegreeAssortativity +

                        AverageDegree +

                        GiniTransitivity +

                        GiniCloseness + GiniDegreeDistribution + GiniBetweenness)


#########################################################
##### References
#
# Stacking Models for Nearly Optimal Link Prediction in Complex Networks
# Amir Ghasemian and Homa Hosseinmardi and Aram Galstyan and Edoardo M. Airoldi and Aaron Clauset
# 2020
# https://github.com/Aghasemian/OptimalLinkPrediction

# Netzschleuder: network catalogue, repository and centrifuge
# Tiago de Paula Peixoto
# 2020
# https://git.skewed.de/count0/netzschleuder
#
# The Colorado Index of Complex Networks
# Aaron Clauset and Ellen Tucker and Matthias Sainz
# 2016
# https://icon.colorado.edu/
#
# SNAP Datasets: Stanford Large Network Dataset Collection
# Jure Leskovec and Andrej Krevl
# 2014
# http://snap.stanford.edu/data

################################################ End of script ################################################


































