#### logistic regression model ####
#### load in master
master_data <- fread("removed_loops/output/master_measures_removed_loops.csv")

# ensure the results are repeatable
set.seed(1234)
# calculate correlation matrix
correlationMatrix <- cor(master_data[,4:24])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.75)
# print indexes of highly correlated attributes
names(master_data[,4:24])[print(highlyCorrelated)]

#### correlation matrix
cor <- cor(master_data[, c(4:24, 32:33)], use = "complete.obs")

corrplot(cor, method = "color", type = "upper", tl.col = "black", tl.offset = 0.4,
         cl.align.text = "l", tl.srt = 90, addgrid.col = "black")

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
scaled_network_glm <- glm(NetworkDomain ~ GiniDegreeCount + GiniBetweenness + GiniTransitivity +
                            GiniEigenvectorCentrality + GiniCloseness +
                            MedianDegree + AverageDegree + AveragePathLength +
                            AverageTransitivity + BetweennessCentrality + ClosenessCentrality +
                            Complexity + DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
                            Density + EigenvectorCentrality + Entropy +
                            GlobalTransitivity + Nodes + Edges,
                          family = binomial, data = master_data
)

scaled_network_glm_removed_corr <- glm(NetworkDomain ~ GiniDegreeCount +
                                         GiniEigenvectorCentrality + GiniCloseness +
                                         AverageDegree + AveragePathLength +
                                         AverageTransitivity + BetweennessCentrality +
                                         ClosenessCentrality +
                                         DegreeAssortativity + DegreeCentrality +
                                         GiniDegreeDistribution +
                                         Density +
                                         Nodes + Edges,
                          family = binomial, data = master_data
)

#scaled_network_glm <- glm(NetworkDomain ~ GiniDegreeCount +
#                            GiniEigenvectorCentrality + GiniCloseness +
#                            MedianDegree + AveragePathLength + Complexity +
#                            BetweennessCentrality + ClosenessCentrality +
#                            Complexity + DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
#                            Density + GlobalTransitivity + Nodes,
#                          family = binomial, data = data.frame(NetworkDomain = master_data$NetworkDomain,
#                                                               apply(master_data[, 3:24], 2, scale))
#)



summary(scaled_network_glm)
summary(scaled_network_glm_removed_corr)
car::vif(scaled_network_glm)
car::vif(scaled_network_glm_removed_corr)
car::vif(network_glm)


## Train classifier
network_glm <- train(NetworkDomain ~ GiniDegreeCount + GiniBetweenness + GiniTransitivity +
                       GiniEigenvectorCentrality + GiniCloseness +
                       MedianDegree + AverageDegree + AveragePathLength +
                       AverageTransitivity + BetweennessCentrality + ClosenessCentrality +
                       Complexity + DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
                       Density + EigenvectorCentrality + Entropy +
                       GlobalTransitivity + Nodes + Edges, data = master_data,
                     method = "glm",
                     family = "binomial"
)

## subset coefficients of logistic regression
coeff <- data.frame(summary(network_glm)$coefficients)
table <- data.table(Variable = rownames(coeff), coeff)
table <- table[-1]
table <- table[order(abs(table$Estimate), decreasing = T)]
table

## plot standardized coefficients, ranked by value
ggplot(table, aes(x = reorder(Variable, abs(Estimate)), y = Estimate)) +
  geom_col(fill="orangered1", width=0.7) + coord_flip() +
  scale_y_continuous(limits=c(-4,3), breaks=c(-4, -2, 0, 1, 2, 3)) + theme(legend.position = "none",
                                                                        panel.background = element_blank(),
                                                                        panel.grid.major = element_line(colour = "gray85",
                                                                                                        size = 0.5),
                                                                        panel.grid.minor = element_line(colour = "gray85",
                                                                                                        size = 0.5),
                                                                        axis.ticks = element_blank(),
                                                                        axis.title.y = element_text(size = 9),
                                                                        axis.title.x = element_text(size = 9)) +
  ylab("Standardized Coefficient") + xlab("Variable")

## plot standardized coffficients, ranked by importance from "varImp()"
imp <- data.table(Variable = rownames(varImp(network_glm, scale = F)$importance),
                Importance = varImp(network_glm, scale = F)$importance$Overall)

res <- merge(imp, table, by = "Variable")

ggplot(res, aes(x = reorder(Variable, Importance), y = Estimate)) +
  geom_col(fill="orangered1", width=0.7) + coord_flip() +  ylab("Standardized Coefficient") +
  xlab("Variable") +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray85",
                                        size = 0.5),
        panel.grid.minor = element_line(colour = "gray85",
                                        size = 0.5),
        axis.ticks = element_blank(),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9)) +
  geom_hline(yintercept = 0, color = "black", size=0.5)


?varImp
