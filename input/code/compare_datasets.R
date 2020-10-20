##########################################
## Comparing datasets
## Mattia Girardi
## 20.09.2020
#########################################

# set working directory
setwd("~/Desktop/Bachelor Thesis/code/bachelor_thesis")

# load in datasets
netzschleuder_essentials <- fread("input/import_datasets/netzschleuder_essentials.csv")
ICON_data <- fread("input/import_datasets/ICON_data.csv")
OLP_essentials <- fread("input/import_datasets/OLP_essentials.csv")

master <- rbind(netzschleuder_essentials, ICON_data, OLP_essentials)

master$network_name <- gsub("_", " ", master$network_name)

tolower(master$network_name)

network_names <- str_extract(master$network_name, '[A-Za-z0-9]+')
network_names_sub <- master$network_name


str_extract(master$network_name, '[A-Za-z]+')

i <- 627
network_names[i]
grep(network_names[i], network_names_sub, value = T)

g <- c("aishiksakdjfhkaljfhs")
h <- c("aishik")

grep(h, g, value=T)

