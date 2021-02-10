######## install packages ########
list.of.packages <- c("data.table", "igraph", "jsonlite", "ggplot2", "gridExtra", "mlbench",
                      "caret", "e1071", "corrplot")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

###### load in diffusion results
diff_master_data <- fread("removed_loops/diffusion/consolidated/1% starting_50% prob_50% threshold.csv")
diff_master_data <- diff_master_data[diff_master_data$Name %in% master_data$Name]
diff_master_data2 <- fread("removed_loops/diffusion/consolidated/1% starting_100% prob_50% threshold.csv")
diff_master_data2 <- diff_master_data2[diff_master_data2$Name %in% master_data$Name]
diff_master_data3 <- fread("removed_loops/diffusion/consolidated/1% starting_100% prob_70% threshold.csv")
diff_master_data3 <- diff_master_data3[diff_master_data3$Name %in% master_data$Name]

for(i in 1:nrow(diff_master_data)){
  if(diff_master_data[i, NetworkDomain] == "Social,Offline"){
    diff_master_data[i, "NetworkDomain"] <- gsub(".*", "Social,Offline", diff_master_data[i, "NetworkDomain"])
  } else if (diff_master_data[i, NetworkDomain] == "Social,Online") {
    diff_master_data[i, "NetworkDomain"] <- gsub(".*", "Social,Online", diff_master_data[i, "NetworkDomain"])
  } else {
    diff_master_data[i, "NetworkDomain"] <- gsub(".*", "Non-Social", diff_master_data[i, "NetworkDomain"])
  }
}


ggplot() +
  geom_point(data = diff_master_data, aes(x = Nodes, y = Iteration_1, color = NetworkDomain),
             size = 0.5) +
  geom_point(data = diff_master_data, aes(x = Nodes, y = Iteration_2, color = NetworkDomain),
             size = 0.5) +
  geom_point(data = diff_master_data, aes(x = Nodes, y = Iteration_3, color = NetworkDomain),
             size = 0.5) +
  geom_point(data = diff_master_data, aes(x = Nodes, y = Iteration_4, color = NetworkDomain),
             size = 0.5) +
  geom_point(data = diff_master_data, aes(x = Nodes, y = Iteration_5, color = NetworkDomain),
             size = 0.5) +
  geom_point(data = diff_master_data, aes(x = Nodes, y = Iteration_6, color = NetworkDomain),
             size = 0.5) +
  geom_point(data = diff_master_data, aes(x = Nodes, y = Iteration_7, color = NetworkDomain),
             size = 0.5) +
  geom_point(data = diff_master_data, aes(x = Nodes, y = Iteration_8, color = NetworkDomain),
             size = 0.5) +
  geom_point(data = diff_master_data, aes(x = Nodes, y = Iteration_9, color = NetworkDomain),
             size = 0.5) +
  geom_point(data = diff_master_data, aes(x = Nodes, y = Iteration_10, color = NetworkDomain),
             size = 0.5) +
  geom_line(data = diff_master_data, aes(x = Nodes, y = Mean),
             size = 0.2, color = "black") +
  theme(legend.position = "hide") + scale_x_log10() + scale_y_log10()


# load in measures data
master_data <- fread("removed_loops/output/master_measures_removed_loops.csv")
master_data <- na.omit(master_data)

table(master_data$NetworkDomain)


# ensure the results are repeatable
set.seed(1234)
# calculate correlation matrix
correlationMatrix <- cor(master_data[,4:27])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.75)
# print indexes of highly correlated attributes
names(master_data[,4:27])[print(highlyCorrelated)]

#### correlation matrix
cor <- cor(master_data[, 4:27], use = "complete.obs")

corrplot(cor, method = "color", type = "upper", tl.col = "black", tl.offset = 0.4,
         cl.align.text = "l", tl.srt = 90, addgrid.col = "black")
corrplot.mixed(cor, tl.pos="lt", diag="n", tl.col="blue", tl.srt=45)

# load in measures data
master_data <- fread("removed_loops/output/master_measures_removed_loops.csv")
master_data <- na.omit(master_data)

# load in diffusion results
D_1_50_50 <- fread("removed_loops/diffusion/consolidated/1% starting_50% prob_50% threshold.csv")
D_1_50_50 <- D_1_50_50[D_1_50_50$Name %in% master_data$Name]
D_1_100_50 <- fread("removed_loops/diffusion/consolidated/1% starting_100% prob_50% threshold.csv")
D_1_100_50 <- D_1_100_50[D_1_100_50$Name %in% master_data$Name]
D_1_100_70 <- fread("removed_loops/diffusion/consolidated/1% starting_100% prob_70% threshold.csv")
D_1_100_70 <- D_1_100_70[D_1_100_70$Name %in% master_data$Name]
D_5_50_70 <- fread("removed_loops/diffusion/consolidated/5% starting_50% prob_70% threshold.csv")
D_5_50_70 <- D_5_50_70[D_5_50_70$Name %in% master_data$Name]
D_5_100_70 <- fread("removed_loops/diffusion/consolidated/5% starting_100% prob_70% threshold.csv")
D_5_100_70 <- D_5_100_70[D_5_100_70$Name %in% master_data$Name]
# add mean to measure table
master_data$D_1_50_50 <- D_1_50_50$Mean
master_data$D_1_100_50 <- D_1_100_50$Mean
master_data$D_1_100_70 <- D_1_100_70$Mean
master_data$D_5_50_70 <- D_5_50_70$Mean
master_data$D_5_100_70 <- D_5_100_70$Mean
summary(master_data)
which(is.na(master_data$D_1_50_50))
table(D_1_50_50[which(is.na(D_1_50_50$Mean)), Domain])
table(D_1_100_50[which(is.na(D_1_100_50$Mean)), NetworkDomain])
table(D_1_100_70[which(is.na(D_1_100_70$Mean)), NetworkDomain])
table(D_5_50_70[which(is.na(D_5_50_70$Mean)), NetworkDomain])
table(D_5_100_70[which(is.na(D_5_100_70$Mean)), NetworkDomain])

master_data <- na.omit(master_data)
master_data[, 4:29] <- data.table(apply(master_data[, 4:29], 2, scale))

## diffusion regression
diffusion_lm <- train(cbind(D_1_50_50, D_1_100_50, D_1_100_70, D_5_50_70, D_5_100_70) ~
                        GiniDegreeCount + GiniBetweenness +
                        GiniEigenvectorCentrality + GiniCloseness +
                        MedianDegree + AverageDegree + AveragePathLength +
                        AverageTransitivity + BetweennessCentrality + ClosenessCentrality +
                        Complexity + DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
                        Density + Entropy +
                        GlobalTransitivity + Nodes + Edges, data = master_data, method="lvq",
                      preProcess="scale", trControl=trainControl(method = "cv", number = 5))

diffusion_lm <- lm(cbind(D_1_50_50, D_1_100_50, D_1_100_70, D_5_50_70, D_5_100_70) ~
                     GiniDegreeCount +
                     GiniEigenvectorCentrality + GiniCloseness +
                     MedianDegree + AveragePathLength + Complexity +
                     BetweennessCentrality + ClosenessCentrality +
                     Complexity + DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
                     Density + GlobalTransitivity + Nodes, data = master_data)

diffusion_lm <- glm(D_1_50_50 ~
                      GiniDegreeCount +
                      GiniDegreeCount + GiniBetweenness + GiniTransitivity +
                      GiniEigenvectorCentrality + GiniCloseness +
                      MedianDegree + AverageDegree + AveragePathLength +
                      AverageTransitivity + BetweennessCentrality + ClosenessCentrality +
                      Complexity + DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
                      Density + EigenvectorCentrality + Entropy +
                      GlobalTransitivity + Nodes + Edges, data = master_data)

car::vif(diffusion_lm)
summary(diffusion_lm)

response <- summary(diffusion_lm)
R1 <- data.frame(response$`Response D_1_50_50`$coefficients)
R1 <- data.table(ID = 1:(nrow(R1)-1), Variable = rownames(R1)[-1], data.table(R1)[-1])
R1 <- R1[order(abs(R1$Estimate), decreasing = T)]
R2 <- data.frame(response$`Response D_1_100_50`$coefficients)
R2 <- data.table(ID = 1:(nrow(R2)-1), Variable = rownames(R2)[-1], data.table(R2)[-1])
R2 <- R2[order(abs(R2$Estimate), decreasing = T)]
R3 <- data.frame(response$`Response D_1_100_70`$coefficients)
R3 <- data.table(ID = 1:(nrow(R3)-1), Variable = rownames(R3)[-1], data.table(R3)[-1])
R3 <- R3[order(abs(R3$Estimate), decreasing = T)]
R4 <- data.frame(response$`Response D_5_50_70`$coefficients)
R4 <- data.table(ID = 1:(nrow(R4)-1), Variable = rownames(R4)[-1], data.table(R4)[-1])
R4 <- R4[order(abs(R4$Estimate), decreasing = T)]
R5 <- data.frame(response$`Response D_5_100_70`$coefficients)
R5 <- data.table(ID = 1:(nrow(R5)-1), Variable = rownames(R5)[-1], data.table(R5)[-1])
R5 <- R5[order(abs(R5$Estimate), decreasing = T)]

table <- head(data.frame(D_1_50_50 = R1$ID, D_1_100_50 = R2$ID, D_1_100_70 = R3$ID,
                     D_5_50_70 = R4$ID, D_5_100_70 = R5$ID), n = 5)

res <- data.table(D_1_50_50 = R1$Variable, D_1_100_50 = R2$Variable, D_1_100_70 = R3$Variable,
                  D_5_50_70 = R4$Variable, D_5_100_70 = R5$Variable)

estimate <- data.table(Measure = rownames(R1)[-1],D_1_50_50 = R1$Estimate[-1], D_1_100_50 = R2$Estimate[-1],
                       D_1_100_70 = R3$Estimate[-1], D_5_50_70 = R4$Estimate[-1],
                       D_5_100_70 = R5$Estimate[-1])

estimate_table <- melt(estimate, id.vars = "Measure")
ggplot(estimate_table, aes(x = reorder(Measure, abs(value)), y = value, fill = variable)) +
  geom_col(position = "dodge") + coord_flip() + labs(fill = "Simulation") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray20",
                                        size = 0.1),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(color = "black"))


cor(table, method = "spearman") %>%
  corrplot(method = "number", type = "upper", tl.col = "black", tl.offset = 0.4,
           cl.align.text = "l", tl.srt = 90, addgrid.col = "black")

if (!requireNamespace("BiocManager", quietly = TRUE))install.packages("BiocManager")
BiocManager::install("gespeR")
library(gespeR)

rank_corr_plot <- matrix(nrow = 5, ncol = 5)
list <- c("D_1_50_50", "D_1_100_50", "D_1_100_70", "D_5_50_70", "D_5_100_70")
for(i in 1:length(list)){
  for(j in 1:length(list)){
    a <- table[, list[i]]
    b <- table[, list[j]]
    names(a) <- names(b) <- LETTERS[1:5]
    rank_corr_plot[i, j] <- rbo(a, b, p = 0.1)
  }
}
colnames(rank_corr_plot) <- rownames(rank_corr_plot) <- list
corrplot(rank_corr_plot, method = "number", type = "upper", tl.col = "black", tl.offset = 0.4,
         cl.align.text = "l", tl.srt = 90, addgrid.col = "black")
?rbo
rbo(a, b, p = 0.9)




