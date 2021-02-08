#### logistic regression model ####
#### load in master
master_data <- fread("removed_loops/output/master_measures_removed_loops.csv")

#### transform Social,Offline and Social,Online into logical variables
for(i in 1:length(master_data$ID)){
  if(master_data[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master_data[i, "NetworkDomain"] <- gsub(".*", "1", master_data[i, "NetworkDomain"])
  } else {
    master_data[i, "NetworkDomain"] <- gsub(".*", "0", master_data[i, "NetworkDomain"])
  }
}

master_data$NetworkDomain <- as.numeric(master_data$NetworkDomain)
master_data$NetworkDomain <- as.factor(master_data$NetworkDomain)
#### logistic regression model
scaled_network_glm <- glm(NetworkDomain ~ GiniDegreeCount +
                            GiniEigenvectorCentrality + GiniCloseness +
                            MedianDegree + AveragePathLength + Complexity +
                            BetweennessCentrality + ClosenessCentrality +
                            Complexity + DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
                            Density + GlobalTransitivity + Nodes + Edges,
                          family = binomial, data = data.frame(NetworkDomain = master_data$NetworkDomain,
                                                               apply(master_data[, 3:24], 2, scale))
)

network_glm <- train(NetworkDomain ~ GiniDegreeCount +
                       GiniEigenvectorCentrality + GiniCloseness +
                       MedianDegree + AveragePathLength + Complexity +
                       BetweennessCentrality + ClosenessCentrality +
                       Complexity + DegreeAssortativity + DegreeCentrality + GiniDegreeDistribution +
                       Density + GlobalTransitivity + Nodes + Edges, data = data.frame(NetworkDomain = master_data$NetworkDomain,
                                                                                       apply(master_data[, 4:24], 2, scale)),
                     trControl = trainControl(method = "cv", number = 5),
                     method = "glm",
                     family = "binomial"
)

summary(scaled_network_glm)
summary(network_glm)
car::vif(scaled_network_glm)
car::vif(network_glm)

coeff <- data.frame(summary(network_glm)$coefficients)
table <- data.table(Variable = rownames(coeff), coeff)
table <- table[-1]
table <- table[order(abs(table$Estimate), decreasing = T)]
table
table[Pr...z.. <= 0.05]

ggplot(table[1:14], aes(x = reorder(Variable, abs(Estimate)), y = Estimate)) +
  geom_col(fill="gray20", width=0.7) + coord_flip() +
  scale_y_continuous(limits=c(-4,4), breaks=c(-4, -2, 0, 2, 4)) + theme(legend.position = "none",
                                                                        panel.background = element_blank(),
                                                                        panel.grid.major = element_line(colour = "gray85",
                                                                                                        size = 0.5),
                                                                        panel.grid.minor = element_line(colour = "gray85",
                                                                                                        size = 0.5),
                                                                        axis.ticks = element_blank(),
                                                                        axis.title.y = element_text(size = 9),
                                                                        axis.title.x = element_text(size = 9)) +
  ylab("Standardized Coefficient") + xlab("Variable")


plot(varImp(network_glm, scale = F))
plot(varImp(scaled_network_glm))