#### acquire Netzschleuder networks data

## install packages
install.packages("jsonlite")
library(jsonlite)

netzschleuder_list <- fromJSON("input/nets.json")
netzschleuder_list[[1]][6]
## input data



https://networks.skewed.de/net/name/files/name.csv.zip



master <- as.character(unzip("/Users/mattia/Downloads/amazon_ratings.csv.zip", list = TRUE)$Name)
data <- read.csv(unz("/Users/mattia/Downloads/amazon_ratings.csv.zip", "edges.csv"), header = TRUE,
                 sep = ",")
