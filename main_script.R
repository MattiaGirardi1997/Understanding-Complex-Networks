##########################################
## Main Script
## Mattia Girardi
## 24.02.2020
#########################################

# set working directory
setwd("~/Desktop/Bachelor Thesis/code/bachelor_thesis")

#install packages
list.of.packages <- c("data.table", "igraph", "jsonlite", "ggplot2", "gridExtra", "mlbench",
                      "caret", "e1071", "corrplot")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

#### 1) Imported data in R scripts from "input/code/import_netzschleuder_data.R" and
#       "input/code/import_OLP_data.R"; saved in "data/all_data"

#### 2) Removed loops, simplified networks in script "data/code/removed_loops.R";
#       results were saved to "data/final_data"; the network specifications were saved in
#      "output/network_specs.csv"

#### 3) Computed measures in script "data/code/removed_loops.R" and saved them in
#      "output/master_measures.csv"

#### 4) Ran diffusion simulation in "output/code/run_diffusion.R" and saved results in "output/diffusion"

#### 5) Consolidated diffusion results in "output/code/epidemic_model/consolidated_diffusion.R" and saved as
#       "output/master_diffusion.csv"

#### 6) create descriptives with "output/code/descriptives.R"

#### 7) create measure corrplot and identify highly correlated indices ####
#### correlation matrix
master_data <- fread("output/master_measures.csv")

### create corrplot
cor <- cor(master_data[, 4:ncol(master_data)], use = "complete.obs")

corrplot(cor, method = "color", tl.col = "black", tl.offset = 0.5,
         cl.align.text = "l", addgrid.col = "black", tl.cex = 0.9)

### investigate correlation
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




#### 8) Machine Learning ####
# 8.1) Classification ####
#### load in master
master_data <- fread("output/master_measures.csv")

#### transform Social,Offline and Social,Online into factor variables with 2 levels
for(i in 1:nrow(master_data)){
  if(master_data[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master_data[i, "NetworkDomain"] <- gsub(".*", "1", master_data[i, "NetworkDomain"])
  } else {
    master_data[i, "NetworkDomain"] <- gsub(".*", "0", master_data[i, "NetworkDomain"])
  }
}

master_data$NetworkDomain <- as.factor(master_data$NetworkDomain)

# standardization
master_data[, 4:length(master_data)] <- data.table(apply(master_data[, 4:length(master_data)],
                                                         2, scale))

# parting data into 95% trainig data and 5% test data
index <- createDataPartition(master_data$NetworkDomain, times = 10000, p=0.95, list=FALSE)

# training classifier and predicting test data
set.seed(1234)

accuracy <- c()
for(i in 1:ncol(index)){
  master_training <- master_data[index[, i],]
  master_test <- master_data[-index[, i],]
  default_glm_mod <- train(
    form = NetworkDomain ~
      Nodes + AveragePathLength + DegreeAssortativity +

      AverageDegree +

      GiniTransitivity +

      GiniCloseness + GiniDegreeDistribution + GiniBetweenness,
    data = master_training,
    method = "glm",
    family = "binomial"
  )
  prediction <- predict(default_glm_mod, newdata = master_test)
  result <- table(master_test$NetworkDomain, prediction)
  accuracy[length(accuracy) + 1] <- (result[1]+result[4])/(nrow(master_test))
}

### Results
sd(accuracy)
mean(accuracy)

# SD : [1] 0.05098981
# mean : [1] 0.7659468






# 8.2) Feature importance; logistic regression ####
master_data <- fread("output/master_measures.csv")

#### transform Social,Offline and Social,Online into factor variables with 2 levels
for(i in 1:nrow(master_data)){
  if(master_data[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
    master_data[i, "NetworkDomain"] <- gsub(".*", "1", master_data[i, "NetworkDomain"])
  } else {
    master_data[i, "NetworkDomain"] <- gsub(".*", "0", master_data[i, "NetworkDomain"])
  }
}

master_data$NetworkDomain <- as.factor(master_data$NetworkDomain)

# standardization
master_data[, 4:length(master_data)] <- data.table(apply(master_data[, 4:length(master_data)],
                                                         2, scale))
logit_glm <- glm(NetworkDomain ~
                     Nodes + AveragePathLength + DegreeAssortativity +

                     AverageDegree +

                     GiniTransitivity +

                     GiniCloseness + GiniDegreeDistribution + GiniBetweenness,

                   family = binomial, data = master_data
)
summary(logit_glm)

## subset coefficients of logistic regression
coeff <- data.frame(summary(logit_glm)$coefficients)
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




#### 9) Epidemic Model ####
# 9.1) Diffusion descriptives ####
# load diffusion results
D_01_100_50 <- fread("output/diffusion/consolidated/0.1% starting_100% prob_50% threshold.csv")
D_1_50_50 <- fread("output/diffusion/consolidated/1% starting_50% prob_50% threshold.csv")
D_1_100_50 <- fread("output/diffusion/consolidated/1% starting_100% prob_50% threshold.csv")
D_1_50_70 <- fread("output/diffusion/consolidated/1% starting_50% prob_70% threshold.csv")
D_1_100_70 <- fread("output/diffusion/consolidated/1% starting_100% prob_70% threshold.csv")
D_5_50_70 <- fread("output/diffusion/consolidated/5% starting_50% prob_70% threshold.csv")
D_5_100_70 <- fread("output/diffusion/consolidated/5% starting_100% prob_70% threshold.csv")

#### creating diffusion plot
files <- lapply(list(D_01_100_50, D_1_50_50, D_1_100_50, D_1_50_70, D_1_100_70,
                     D_5_50_70, D_5_100_70), split_domains)

specs <- c("0.1% starting, 100% probabillity of transmission, 50% threshold",
           "1% starting, 50% probabillity of transmission, 50% threshold",
           "1% starting, 100% probabillity of transmission, 50% threshold",
           "1% starting, 50% probabillity of transmission, 70% threshold",
           "1% starting, 100% probabillity of transmission, 70% threshold",
           "5% starting, 50% probabillity of transmission, 70% threshold",
           "5% starting, 100% probabillity of transmission, 70% threshold")

for(i in 1:length(specs)){
  files[[i]]$Domain <- factor(files[[i]][, NetworkDomain], levels=c("Social", "Non-Social"))
  var <- paste(letters[i])
  assign(var, ggplot(files[[i]], aes(x = Edges, y = Mean)) +
           geom_point(aes(color = NetworkDomain)) +
           scale_x_log10(breaks = c(100, 1000, 10000)) +
           scale_y_log10(labels = function(x) format(x, scientific = FALSE)) + ylab("Number of Iterations") +
           scale_color_manual(values = c("gray20", "orangered1")) + labs(title = sprintf("%s", specs[i])) +
           facet_wrap(Domain ~ ., scales = "free_x",nrow = 2) + theme(panel.background = element_blank(),
                                                                      panel.grid.major = element_line(colour = "gray85", size = 1),
                                                                      panel.grid.minor = element_line(colour = "gray85"),
                                                                      axis.ticks = element_blank(),
                                                                      axis.title.y = element_text(size = 20),
                                                                      axis.title.x = element_text(size = 25),
                                                                      axis.text.x = element_text(size = 20, colour = "Black", angle = 45, hjust = 1),
                                                                      axis.text.y = element_text(size = 20, colour = "Black", angle = 45, hjust = 1),
                                                                      legend.position = "none",
                                                                      plot.title = element_text(size = 20, hjust = 0.5, face = "bold", vjust = 5),
                                                                      strip.background = element_rect(
                                                                        fill="white"),
                                                                      strip.text = element_text(size = 20, face = "bold"),
                                                                      axis.line = element_line(),
                                                                      axis.title.x.bottom = element_blank(),
                                                                      plot.margin=unit(c(2,2,2,2),"cm")
           )
  )
  if(i == length(specs)){
    grid.arrange(a, b, c, d, e, f, g)
  }
}






# 9.2) Spread of diffusion results ####
# load diffusion results
D_01_100_50 <- fread("output/diffusion/consolidated/0.1% starting_100% prob_50% threshold.csv")
D_1_50_50 <- fread("output/diffusion/consolidated/1% starting_50% prob_50% threshold.csv")
D_1_100_50 <- fread("output/diffusion/consolidated/1% starting_100% prob_50% threshold.csv")
D_1_50_70 <- fread("output/diffusion/consolidated/1% starting_50% prob_70% threshold.csv")
D_1_100_70 <- fread("output/diffusion/consolidated/1% starting_100% prob_70% threshold.csv")
D_5_50_70 <- fread("output/diffusion/consolidated/5% starting_50% prob_70% threshold.csv")
D_5_100_70 <- fread("output/diffusion/consolidated/5% starting_100% prob_70% threshold.csv")

## creating spread plot
files <- lapply(list(D_01_100_50, D_1_50_50, D_1_100_50, D_1_50_70, D_1_100_70,
                     D_5_50_70, D_5_100_70), split_domains)

# calculate spread for each specification
files[[1]][, "Rate"] <- (apply(files[[1]][,5:14], 1, max) - apply(files[[1]][,5:14], 1, min))/
  files[[1]][, 3]
files[[2]][,"Rate"] <- (apply(files[[2]][,5:14], 1, max) - apply(files[[2]][,5:14], 1, min))/
  files[[2]][, 3]
files[[3]][,"Rate"] <- (apply(files[[3]][,5:14], 1, max) - apply(files[[3]][,5:14], 1, min))/
  files[[3]][, 3]
files[[4]][,"Rate"] <- (apply(files[[4]][,5:14], 1, max) - apply(files[[4]][,5:14], 1, min))/
  files[[4]][, 3]
files[[5]][,"Rate"] <- (apply(files[[5]][,5:14], 1, max) - apply(files[[5]][,5:14], 1, min))/
  files[[5]][, 3]
files[[6]][,"Rate"] <- (apply(files[[6]][,5:14], 1, max) - apply(files[[6]][,5:14], 1, min))/
  files[[6]][, 3]
files[[7]][, "Rate"] <- (apply(files[[7]][,5:14], 1, max) - apply(files[[7]][,5:14], 1, min))/
  files[[7]][, 3]

specs <- c("0.1% starting, 100% probabillity of transmission, 50% threshold",
           "1% starting, 50% probabillity of transmission, 50% threshold",
           "1% starting, 100% probabillity of transmission, 50% threshold",
           "1% starting, 50% probabillity of transmission, 70% threshold",
           "1% starting, 100% probabillity of transmission, 70% threshold",
           "5% starting, 50% probabillity of transmission, 70% threshold",
           "5% starting, 100% probabillity of transmission, 70% threshold")



for(i in 1:length(specs)){
  files[[i]]$Domain <- factor(files[[i]][, NetworkDomain], levels=c("Non-Social", "Social"))
  var <- paste(letters[i])
  assign(var, ggplot(files[[i]], aes(x = Nodes, y = Rate, color = NetworkDomain)) + geom_point() +
           scale_x_log10() + facet_wrap(NetworkDomain~.) + scale_y_continuous(limits = c(0, 50)) +
           scale_color_manual(values = c("gray20", "orangered1")) + ylab("Relative spread") +
           labs(title = sprintf("%s", specs[i])) +
           theme(panel.background = element_blank(),
                 panel.grid.major = element_line(colour = "gray85", size = 1),
                 panel.grid.minor = element_line(colour = "gray85"),
                 axis.ticks = element_blank(),
                 axis.title.y = element_text(size = 25, face = "plain"),
                 axis.title.x = element_text(size = 25, face = "plain"),
                 axis.text.x = element_text(size = 20, colour = "Black", angle = 45, hjust = 1),
                 axis.text.y = element_text(size = 20, colour = "Black", angle = 45, hjust = 1),
                 legend.position = "none",
                 plot.title = element_text(size = 20, hjust = 0.5, vjust = 5),
                 strip.background = element_rect(
                   fill="white"),
                 strip.text = element_text(size = 20),
                 axis.line = element_line(),
                 axis.title.x.bottom = element_blank(),
                 plot.margin=unit(c(2,2,2,2),"cm"))
  )
  if(i == length(specs)){
    grid.arrange(a, b, c, d, e, f, g, nrow = 3, ncol = 3)
  }
}
# 9.3) Feature importance; diffusion regression ####
# load in measures data
master_diffusion <- fread("output/master_diffusion.csv")

# standardization
master_diffusion[, 4:length(master_data)] <- data.table(apply(master_diffusion[, 4:length(master_data)],
                                                         2, scale))

# diffusion regression
diffusion_lm <- lm(cbind(D_01_100_50, D_1_50_50, D_1_100_50, D_1_50_70, D_1_100_70,
                         D_5_50_70, D_5_100_70) ~
                     Nodes + AveragePathLength + DegreeAssortativity +

                     AverageDegree +

                     GiniTransitivity +

                     GiniCloseness + GiniDegreeDistribution + GiniBetweenness,
                   data = master_diffusion)
summary(diffusion_lm)

# calculate mean of R-squared
r <- c()
for(i in 1:length(summary(diffusion_lm))){
  r[length(r) + 1] <- summary(diffusion_lm)[[i]]$r.squared
  if(i == length(summary(diffusion_lm))){
    print(mean(r))
  }
}

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

### creating rank corrplot
# creating summary data table for each regression
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

#### Jaccard rank correlation
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



































