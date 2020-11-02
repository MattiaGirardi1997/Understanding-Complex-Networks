##########################################
## Logistic Regression
## Mattia Girardi
## 02.11.2020
#########################################

# set working directory
setwd("~/Desktop/Bachelor Thesis/code/bachelor_thesis")

# install packages
list.of.packages <- c("data.table")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)


#### load in master
master_measures <- fread("output/master_measures.csv")

#### transform Social,Offline and Social,Online into logical variables
for(i in 1:length(master_measures$ID)){
  if(master_measures[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master_measures[i, "NetworkDomain"] <- gsub(".*", "TRUE", master_measures[i, "NetworkDomain"])
  } else {
    master_measures[i, "NetworkDomain"] <- gsub(".*", "FALSE", master_measures[i, "NetworkDomain"])
  }
}

master_measures$NetworkDomain <- as.logical(master_measures$NetworkDomain)

#### logistic regression model
network_glm <- glm(NetworkDomain ~ AverageDegree + AveragePathLength + AverageTransitivity + Betweenness + Closeness +
                    DegreeAssortativity + DegreeDistribution + Density + EigenvectorCentrality + GlobalTransitivity +
                    Reciprocity, family = binomial, data = master_measures)

summary(network_glm)

master_measures <- na.omit(master_measures)
glm.probs <- predict(network_glm,type = "response")

mean(glm.probs)

glm.pred <- ifelse(glm.probs > 0.5, "TRUE", "FALSE")

attach(master_measures)
table(glm.pred, NetworkDomain)


