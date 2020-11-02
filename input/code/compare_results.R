##########################################
## Comparing results
## Mattia Girardi
## 01.11.2020
#########################################

# set working directory
setwd("~/Desktop/Bachelor Thesis/code/bachelor_thesis")

# install packages
list.of.packages <- c("data.table")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

#### load in measures
ICON_measures <- fread("output/ICON_measures.csv")
netzschleuder_measures <- fread("output/netzschleuder_measures.csv")
OLP_measures <- fread("output/OLP_measures.csv")
added_networks_measures <- fread("output/added_networks_measures.csv")

#### create master table
master_measures <- rbind(ICON_measures, netzschleuder_measures, OLP_measures, added_networks_measures)

master_measures <- master_measures[order(master_measures$Name)]

write.table(master_measures, file = "output/master_measures.csv", sep = ",", row.names = F)

#### analyze distribution

table(master_measures$NetworkDomain)








