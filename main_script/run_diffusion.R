##########################################
## Running diffusion
## Mattia Girardi
## 23.11.2020
#########################################

# set working directory
setwd("~/Desktop/Bachelor Thesis/code/bachelor_thesis")

# install packages
list.of.packages <- c("data.table", "igraph", "statnet", "tidyverse", "magrittr", "dplyr")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

# load in function
source("R/diffusion_function.R")
master_data <- fread("output/undirected/master_measures_2.csv")[, c("Name", "NetworkDomain", "number_edges")]
master_data[87]
# run diffusion
set.seed(1234)
for(n in 4:5){
  for(j in 1:556){
    simulate.diffusion(j = j, p.infection = 0.5, pct.starting.infected = 0.05, n = n, threshold = 0.7)
  }
}

#######
# load in function
source("R/diffusion_function.R")

master_data <-
  fread("output/undirected/master_measures_2.csv")[number_edges < 500000 & nodes < 45000,
                                                   c("Name", "NetworkDomain", "number_edges")]
n <- 10
for(j in c(266:441,443:535)){
  simulate.diffusion(j = j, p.infection = 0.5, pct.starting.infected = 0.1, n = n, threshold = 0.7)
}
#######
### - topology 442
simulate.diffusion(j = 87, p.infection = 0.5, pct.starting.infected = 0.05, n = n, threshold = 0.7)


#########
diff_1 <- fread("output/diffusion/diffusion_results_1.csv")
diff_2 <- fread("output/diffusion/diffusion_results_2.csv")
diff_3 <- fread("output/diffusion/diffusion_results_3.csv")
diff_4 <- fread("output/diffusion/diffusion_results_4.csv")
diff_6 <- fread("output/diffusion/diffusion_results_6.csv")
diff_7 <- fread("output/diffusion/diffusion_results_7.csv")

master <- data.table(diff_1$Domain, diff_1$Iterations_1, diff_2$Iterations_1, diff_3$Iterations_1,
                     diff_4$Iterations_1, diff_6$Iterations_1)

master <- data.table(master$V1, sapply(master[, -1], as.numeric))
master <- data.table(master, rowMeans(master[, 2:5]))

names(master) <- c("domain", "diff_1", "diff_2", "diff_3", "diff_4", "diff_6", "Mean")

master$sub <- master$Mean - master$diff_6











master <- data.table(diff_1$Domain, diff_1$Iterations_1, diff_2$Iterations_1, diff_3$Iterations_1,
                     diff_4$Iterations_1, diff_6$Iterations_1,diff_1$Nodes)
names(master) <- c("domain", "diff_1", "diff_2", "diff_3", "diff_4", "diff_6", "Nodes")
master <- data.table(master$domain, sapply(master[, -1], as.numeric))
d <- melt(master[, -6], id.vars=c("V1", "Nodes"))


ggplot(d, aes(x = Nodes, y = value, color = V1)) + geom_point() + stat_smooth()

master_measures_2 <- read_csv("output/undirected/master_measures_2.csv")

master_data = fread("output/undirected/master_measures_2.csv")[number_edges < 500000 & nodes < 45000]

ggplot(master_data, aes(x = nodes, y = number_edges, color = NetworkDomain)) + geom_point()
table(master_data$NetworkDomain)

for(n in 1:5){
  if(n == 1){
    res <- fread("output/diffusion/diffusion_results_1.csv")
    write.table(res, file = "output/diffusion/diffusion_results_complete.csv", sep = ",", row.names = F)
    } else {
    iter <- fread(sprintf("output/diffusion/diffusion_results_%s.csv", n))[, sprintf("Iterations_%s", n)
                                                                           := Iterations][, 5]
    master <- cbind(fread("output/diffusion/diffusion_results_complete.csv"), iter)
    write.table(master, file = "output/diffusion/diffusion_results_complete.csv", sep = ",", row.names = F)
  }
}

ggplot(master, aes(x = Nodes, y = means, fill = Domain)) + geom_point()





data_long <- tidyr::gather(master, key = type_col, value = categories, -c("Domain", "Name", "Nodes"))
ggplot(data_long, aes(x = '', y = categories, col = Name)) +
  geom_point(position = "fill", stat = "identity")









