######## install packages ########
list.of.packages <- c("data.table", "igraph", "jsonlite", "ggplot2", "gridExtra", "mlbench",
                      "caret", "e1071", "corrplot")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

# load in measures data
master_diffusion <- fread("removed_loops/output/master_diffusion.csv")

## diffusion regression
diffusion_lm <- lm(cbind(D_01_100_50, D_1_50_50, D_1_100_50, D_1_50_70, D_1_100_70,
                         D_5_50_70, D_5_100_70) ~
                     Nodes + Edges + AveragePathLength + DegreeAssortativity + Density +

                     AverageTransitivity + GiniTransitivity + GlobalTransitivity +

                     AverageDegree + MedianDegree +

                     Complexity + Entropy +

                     BetweennessCentrality + ClosenessCentrality + DegreeCentrality +
                     EigenvectorCentrality +

                     GiniBetweenness + GiniCloseness + GiniDegreeDistribution +
                     GiniEigenvectorCentrality,
                   data = master_diffusion)
summary(diffusion_lm)

diffusion_lm <- lm(cbind(D_01_100_50, D_1_50_50, D_1_100_50, D_1_50_70, D_1_100_70,
                          D_5_50_70, D_5_100_70) ~
                     Nodes + AveragePathLength + DegreeAssortativity +

                     AverageDegree +

                     GiniTransitivity +

                     GiniCloseness + GiniDegreeDistribution + GiniBetweenness,
                   data = master_diffusion)
summary(diffusion_lm)
car::vif(diffusion_lm)

r <- c()
for(i in 1:length(summary(diffusion_lm))){
  r[length(r) + 1] <- summary(diffusion_lm)[[i]]$r.squared
  print(mean(r))
}



## creating summary data table for each regression
response <- summary(diffusion_lm)
for(i in 1:length(response)){
  var <- paste("R", i, sep = "")
  res <- data.frame(response[[i]]$coefficients)
  res <- data.table(ID = 1:(nrow(res)-1), Variable = rownames(res)[-1], data.table(res)[-1])
  res <- res[order(abs(res$Estimate), decreasing = T)]
  assign(var, res)
}

## create data table based on ID (ranks)
ranks <- data.frame(D_01_100_50 = R1$ID, D_1_50_50 = R2$ID, D_1_100_50 = R3$ID, D_1_50_70 = R4$ID,
                         D_1_100_70 = R5$ID, D_5_50_70 = R6$ID, D_5_100_70 = R7$ID)[1:5,]

res <- data.table(D_01_100_50 = R1$Variable, D_1_50_50 = R2$Variable, D_1_100_50 = R3$Variable,
                  D_1_50_70 = R4$Variable, D_1_100_70 = R5$Variable, D_5_50_70 = R6$Variable,
                  D_5_100_70 = R7$Variable)


#### rank correlation
jaccard <- function(a, b) {
  intersection = length(intersect(a, b))
  union = length(a) + length(b) - intersection
  return (intersection/union)
}
####
list <- c("D_01_100_50", "D_1_50_50", "D_1_100_50", "D_1_50_70","D_1_100_70",
          "D_5_50_70", "D_5_100_70")
rank_corr_plot <- matrix(nrow = length(list), ncol = length(list))
for(i in 1:length(list)){
  for(j in 1:length(list)){
    a <- ranks[, list[i]]
    b <- ranks[, list[j]]
    names(a) <- names(b) <- LETTERS[1:nrow(ranks)]
    rank_corr_plot[i, j] <- jaccard(a, b)
  }
}
colnames(rank_corr_plot) <- rownames(rank_corr_plot) <- list

cor <- cor(ranks, method = "spearman")

rank_corr_plot[lower.tri(rank_corr_plot)] <- cor[lower.tri(cor)]

corrplot(rank_corr_plot, method = "number", tl.col = "black", tl.offset = 0.4,
         cl.align.text = "l", tl.srt = 90, addgrid.col = "black", number.cex = 1.5,
         tl.cex = 1.5)

### plotting standardized coefficients
response <- summary(diffusion_lm)
for(i in 1:length(response)){
  var <- paste("R", i, sep = "")
  res <- data.frame(response[[i]]$coefficients)
  assign(var, res)
}

estimate <- data.table(Measure = rownames(R1)[-1], D_01_100_50 = R1$Estimate[-1],
                       D_1_50_50 = R2$Estimate[-1], D_1_100_50 = R3$Estimate[-1],
                       D_1_50_70 = R4$Estimate[-1], D_1_100_70 = R5$Estimate[-1],
                       D_5_50_70 = R6$Estimate[-1], D_5_100_70 = R7$Estimate[-1])

estimate$Measure <- c("Nodes", "Average Path Length", "Degree Assortativity", "Average Degree",
                      "Gini Transitivity", "Gini Closeness",
                      "Gini Degree Distribution", "Gini Betweenness")


estimate <- estimate[order(abs(estimate$D_1_50_50), decreasing = T)]

estimate_table <- melt(estimate, id.vars = "Measure")

ggplot(estimate_table, aes(x = reorder(Measure, abs(value)), y = value,
                           fill = variable)) + geom_col(position = "dodge") +
  coord_flip() + labs(fill = "Simulation") +
  scale_fill_manual(values = c("mediumpurple2", "seagreen2", "springgreen3", "seagreen4", "darkgreen",
                               "skyblue2", "dodgerblue1")) +
  theme(axis.title = element_blank(),
        axis.text.x.bottom = element_text(size = 12),
        axis.text.y.left = element_text(size = 12),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray85",
                                        size = 0.5),
        panel.grid.minor = element_line(colour = "gray85",
                                        size = 0.5),
        axis.ticks = element_blank(),
        legend.position = c(0.85, 0.4),
        legend.background = element_rect(size = 0.1, colour = "Black"),
        legend.key = element_blank(),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.text = element_text(color = "gray20", size = 12),
        legend.title = element_text(size = 12)) +
  guides(fill = guide_legend(reverse=TRUE))


