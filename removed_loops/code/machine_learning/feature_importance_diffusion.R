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

master_data <- na.omit(master_data)
master_data[, 4:28] <- data.table(apply(master_data[, 4:28], 2, scale))

## diffusion regression
diffusion_lm <- train(cbind(D_1_50_50, D_1_100_50, D_1_100_70, D_5_50_70, D_5_100_70) ~
                        GiniDegreeCount + GiniBetweenness +
                        GiniEigenvectorCentrality + GiniCloseness +
                        MedianDegree + AverageDegree + AveragePathLength +
                        AverageTransitivity + BetweennessCentrality + ClosenessCentrality +
                        Complexity + DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
                        Density + Entropy +
                        GlobalTransitivity + Nodes + Edges, data = master_data, method="glm",
                      preProcess="scale", trControl=trainControl(method = "cv", number = 5))

diffusion_lm <- lm(cbind(D_1_50_50, D_1_100_50, D_1_100_70, D_5_50_70, D_5_100_70) ~
                     GiniDegreeCount +
                     GiniEigenvectorCentrality + GiniCloseness +
                     MedianDegree + AveragePathLength + Complexity +
                     BetweennessCentrality + ClosenessCentrality +
                     Complexity + DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
                     Density + GlobalTransitivity + Nodes, data = master_data)

vif(diffusion_lm)

response <- summary(diffusion_lm)
R1 <- data.frame(response$`Response D_1_50_50`$coefficients)
R1 <- data.table(ID = 1:14, Variable = rownames(R1)[-1], data.table(R1)[-1])
R1 <- R1[order(abs(R1$Estimate), decreasing = T)]
R2 <- data.frame(response$`Response D_1_100_50`$coefficients)
R2 <- data.table(ID = 1:14, Variable = rownames(R2)[-1], data.table(R2)[-1])
R2 <- R2[order(abs(R2$Estimate), decreasing = T)]
R3 <- data.frame(response$`Response D_1_100_70`$coefficients)
R3 <- data.table(ID = 1:14, Variable = rownames(R3)[-1], data.table(R3)[-1])
R3 <- R3[order(abs(R3$Estimate), decreasing = T)]
R4 <- data.frame(response$`Response D_5_50_70`$coefficients)
R4 <- data.table(ID = 1:14, Variable = rownames(R4)[-1], data.table(R4)[-1])
R4 <- R4[order(abs(R4$Estimate), decreasing = T)]
R5 <- data.frame(response$`Response D_5_100_70`$coefficients)
R5 <- data.table(ID = 1:14, Variable = rownames(R5)[-1], data.table(R5)[-1])
R5 <- R5[order(abs(R5$Estimate), decreasing = T)]


table <- head(data.frame(D_1_50_50 = R1$ID, D_1_100_50 = R2$ID, D_1_100_70 = R3$ID,
                         D_5_50_70 = R4$ID, D_5_100_70 = R5$ID), n = 5)

res <- data.table(D_1_50_50 = R1$Variable, D_1_100_50 = R2$Variable, D_1_100_70 = R3$Variable,
                  D_5_50_70 = R4$Variable, D_5_100_70 = R5$Variable)


####
if (!requireNamespace("BiocManager", quietly = TRUE))install.packages("BiocManager")
BiocManager::install("gespeR")
library(gespeR)
####

rank_corr_plot <- matrix(nrow = 4, ncol = 4)
list <- c("D_1_50_50", "D_1_100_50", "D_1_100_70", "D_5_50_70", "D_5_100_70")
for(i in 1:length(list)){
  for(j in 1:length(list)){
    a <- table[, list[i]]
    b <- table[, list[j]]
    names(a) <- names(b) <- LETTERS[1:5]
    rank_corr_plot[i, j] <- rbo(a, b, p = 0.5)
  }
}
colnames(rank_corr_plot) <- rownames(rank_corr_plot) <- list
corrplot(rank_corr_plot, method = "number", type = "upper", tl.col = "black", tl.offset = 0.4,
         cl.align.text = "l", tl.srt = 90, addgrid.col = "black")



ggplot(R1, aes(x = reorder(Variable, abs(Estimate)), y = Estimate)) +
  geom_col(fill="gray20", width=0.7) + coord_flip() +
  scale_y_continuous(limits=c(-1.5,1.5), breaks=c(-1, 0, 1)) + theme(legend.position = "none",
                                                                        panel.background = element_blank(),
                                                                        panel.grid.major = element_line(colour = "gray85",
                                                                                                        size = 0.5),
                                                                        panel.grid.minor = element_line(colour = "gray85",
                                                                                                        size = 0.5),
                                                                        axis.ticks = element_blank(),
                                                                        axis.title.y = element_text(size = 9),
                                                                        axis.title.x = element_text(size = 9)) +
  ylab("Standardized Coefficient") + xlab("Variable")



