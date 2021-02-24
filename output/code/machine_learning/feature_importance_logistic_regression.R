# install packages
list.of.packages <- c("data.table", "dplyr", "mlbench", "caret", "e1071", "corrplot")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

#### logistic regression model ####
#### load in master
master_data <- fread("removed_loops/output/master_measures_removed_loops.csv")
# ensure the results are repeatable
set.seed(1234)
# calculate correlation matrix
correlationMatrix <- cor(master_data[,4:ncol(master_data)])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.75)
# print indexes of highly correlated attributes
names(master_data[,4:ncol(master_data)])[print(highlyCorrelated)]

#### correlation matrix
cor <- cor(master_data[, 4:ncol(master_data)], use = "complete.obs")

corrplot(cor, method = "color", tl.col = "black", tl.offset = 0.5,
         cl.align.text = "l", addgrid.col = "black", tl.cex = 0.9)


#### transform Social,Offline and Social,Online into logical variables
#### load in master
master_data <- fread("removed_loops/output/master_measures_removed_loops.csv")

for(i in 1:length(master_data$ID)){
  if(master_data[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master_data[i, "NetworkDomain"] <- gsub(".*", 1, master_data[i, "NetworkDomain"])
  } else {
    master_data[i, "NetworkDomain"] <- gsub(".*", 0, master_data[i, "NetworkDomain"])
  }
}

master_data$NetworkDomain <- as.factor(master_data$NetworkDomain)
master_data[, 4:length(master_data)] <- data.table(apply(master_data[, 4:length(master_data)],
                                                         2, scale))

#### logistic regression model
network_glm <- glm(NetworkDomain ~
                            Nodes + Edges + AveragePathLength + DegreeAssortativity + Density +

                            AverageTransitivity + GiniTransitivity + GlobalTransitivity +

                            AverageDegree + MedianDegree +

                            Complexity + Entropy +

                            BetweennessCentrality + ClosenessCentrality + DegreeCentrality +
                            EigenvectorCentrality +

                            GiniBetweenness + GiniCloseness + GiniDegreeDistribution +
                            GiniEigenvectorCentrality,
                          family = binomial, data = master_data
)

network_glm <- glm(NetworkDomain ~
                     Nodes + AveragePathLength + DegreeAssortativity +

                     AverageDegree +

                     GiniTransitivity +

                     GiniCloseness + GiniDegreeDistribution + GiniBetweenness,

                   family = binomial, data = master_data
)
summary(network_glm)
car::vif(network_glm)
mean(car::vif(network_glm))
plot(varImp(network_glm, scale = F))

coeff <- data.frame(summary(network_glm)$coefficients)
table <- data.table(Variable = rownames(coeff), coeff)
table <- table[-1]
table <- table[order(abs(table$Estimate), decreasing = T)]
table$Estimate <- sapply(table$Estimate, exp)
table
ggplot(table, aes(x = reorder(Variable, abs(Estimate)), y = Estimate)) + geom_col() + coord_flip()


## subset coefficients of logistic regression
coeff <- data.frame(summary(network_glm)$coefficients)
table <- data.table(Variable = rownames(coeff), coeff)[-1]
table <- table[order(abs(table$Estimate), decreasing = T)]
table
ggplot(table, aes(x = reorder(Variable, abs(Estimate)), y = Estimate)) + geom_col() + coord_flip()

## plot standardized coffficients
res <- table
names(res)[2] <- "Standardized Coeffiecient"
res <- res[order(-abs(res$'Standardized Coeffiecient'))]
res$'Odds Ratio' <- sapply(table$Estimate, exp)
res$Variable <- c("Gini Betweenness***", "Average Path Length *", "Gini Degree Distrbution*",
                  "Gini Closeness***", "Gini Transitivity*", "Degree Assortativity",
                  "Nodes", "Average Degree")
res$Variable <- factor(res$Variable, levels = rev(c("Gini Betweenness***", "Average Path Length *", "Gini Degree Distrbution*",
                                                    "Gini Closeness***", "Gini Transitivity*", "Degree Assortativity",
                                                    "Nodes", "Average Degree")))

res <- melt(res[, c(1, 2, 6)], id.vars = "Variable")
ggplot(res, aes(x = Variable, y = value, fill = variable)) +
  geom_col(width=0.7, position = "dodge") + coord_flip() +
  scale_fill_manual(values = c("seagreen2", "dodgerblue2")) +
  scale_y_continuous(limits=c(-2,2.5), breaks=c(-2, -1, 0, 1, 2)) +
  theme(legend.title = element_blank(),
        axis.title = element_blank(),
        axis.text.x.bottom = element_text(size = 12),
        axis.text.y.left = element_text(size = 12),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray85",
                                        size = 0.5),
        panel.grid.minor = element_line(colour = "gray85",
                                        size = 0.5),
        axis.ticks = element_blank(),
        legend.position = c(0.85, 0.25),
        legend.background = element_rect(size = 0.1, colour = "Black"),
        legend.key = element_blank(),
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.text = element_text(color = "gray20", size = 12)) +
  geom_hline(yintercept = 1, color = "dodgerblue3", size=1, linetype = "dotted")+
  geom_hline(yintercept = 0, color = "gray20", size=0.5)

