######## install packages ########
list.of.packages <- c("data.table", "igraph", "jsonlite", "ggplot2", "gridExtra", "mlbench",
                      "caret", "e1071", "corrplot")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

# load in measures data
master_data <- fread("removed_loops/output/master_measures_removed_loops.csv")

## transform Social,Offline and Social,Online into logical variables
split_domains <- function(data){
  for(i in 1:nrow(data)){
    if(data[i, NetworkDomain] %in% c("Social,Offline", "Social,Online")){
      data[i, "NetworkDomain"] <- gsub(".*", "Social", data[i, "NetworkDomain"])
    } else {
      data[i, "NetworkDomain"] <- gsub(".*", "Non-Social", data[i, "NetworkDomain"])
    }
  }
  return(data)
}
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

master_data <- master_data[, c(3:4, 25:29)]
master_data <- na.omit(master_data)

master_data$D_1D_2 <- (master_data$D_1_100_50-master_data$D_1_50_50)/(master_data$D_1_50_50)
master_data$D_2D_3 <- (master_data$D_1_100_70-master_data$D_1_100_50)/(master_data$D_1_100_50)
master_data$D_3D_4 <- (master_data$D_5_50_70-master_data$D_1_100_70)/(master_data$D_1_100_70)
master_data$D_4D_5 <- (master_data$D_5_100_70-master_data$D_5_50_70)/(master_data$D_5_50_70)

# analyze spread of diffusion results
ggplot(master_data, aes(x = Nodes, y = D_1D_2, color = NetworkDomain)) + geom_point() +
  scale_x_log10()
ggplot(master_data, aes(x = Nodes, y = D_2D_3, color = NetworkDomain)) + geom_point() +
  scale_x_log10()
ggplot(master_data, aes(x = Nodes, y = D_3D_4, color = NetworkDomain)) + geom_point() +
  scale_x_log10()
ggplot(master_data, aes(x = Nodes, y = D_4D_5, color = NetworkDomain)) + geom_point() +
  scale_x_log10()

files <- lapply(list(D_1_50_50, D_1_100_50, D_1_100_70, D_5_50_70, D_5_100_70), na.omit)

files[[1]][["Rate1"]] <- (apply(files[[1]][,5:14], 1, max) - apply(files[[1]][,5:14], 1, min))/
  apply(files[[1]][,5:14], 1, min)
files[[2]][["Rate2"]] <- (apply(files[[2]][,5:14], 1, max) - apply(files[[2]][,5:14], 1, min))/
  apply(files[[2]][,5:14], 1, min)
files[[3]][["Rate3"]] <- (apply(files[[3]][,5:14], 1, max) - apply(files[[3]][,5:14], 1, min))/
  apply(files[[3]][,5:14], 1, min)
files[[4]][["Rate4"]] <- (apply(files[[4]][,5:14], 1, max) - apply(files[[4]][,5:14], 1, min))/
  apply(files[[4]][,5:14], 1, min)
files[[5]][["Rate5"]] <- (apply(files[[5]][,5:14], 1, max) - apply(files[[5]][,5:14], 1, min))/
  apply(files[[5]][,5:14], 1, min)


ggplot(split_domains(files[[1]]), aes(x = Nodes, y = Rate1, color = NetworkDomain)) + geom_point() +
  scale_x_log10()
ggplot(split_domains(files[[2]]), aes(x = Nodes, y = Rate2, color = NetworkDomain)) + geom_point() +
  scale_x_log10()
ggplot(split_domains(files[[3]]), aes(x = Nodes, y = Rate3, color = NetworkDomain)) + geom_point() +
  scale_x_log10()
ggplot(split_domains(files[[4]]), aes(x = Nodes, y = Rate4, color = NetworkDomain)) + geom_point() +
  scale_x_log10()
ggplot(split_domains(files[[5]]), aes(x = Nodes, y = Rate5, color = NetworkDomain)) + geom_point() +
  scale_x_log10()




files <- lapply(list(D_1_50_50, D_1_100_50, D_1_100_70, D_5_50_70, D_5_100_70), na.omit)

files[[1]]$max <- apply(files[[1]][,5:14], 1, max)
files[[1]]$min <- apply(files[[1]][,5:14], 1, min)
files[[2]]$max <- apply(files[[2]][,5:14], 1, max)
files[[2]]$min <- apply(files[[2]][,5:14], 1, min)
files[[3]]$max <- apply(files[[3]][,5:14], 1, max)
files[[3]]$min <- apply(files[[3]][,5:14], 1, min)
files[[4]]$max <- apply(files[[4]][,5:14], 1, max)
files[[4]]$min <- apply(files[[4]][,5:14], 1, min)
files[[5]]$max <- apply(files[[5]][,5:14], 1, max)
files[[5]]$min <- apply(files[[5]][,5:14], 1, min)


for(i in 1:5){
  var <- paste(letters[i])
  assign(var, ggplot(split_domains(files[[i]]), aes(x = Nodes, y = Mean)) + geom_point(aes(color = NetworkDomain)) +
      geom_errorbar(aes(x = Nodes, ymin = min, ymax = max, color = NetworkDomain)) + scale_x_log10() +
      scale_y_log10(labels = function(x) format(x, scientific = FALSE)) + ylab("Number of Iterations") +
      scale_color_manual(values = c("gray20", "orangered1")) +
      facet_grid(NetworkDomain ~ .) + theme(panel.background = element_blank(),
                                        panel.grid.major = element_line(colour = "gray85", size = 1),
                                        panel.grid.minor = element_line(colour = "gray85"),
                                        axis.ticks = element_blank(),
                                        axis.title.y = element_text(size = 20),
                                        axis.title.x = element_text(size = 25),
                                        axis.text.x = element_text(size = 20, colour = "Black", angle = 45, hjust = 1),
                                        axis.text.y = element_text(size = 20, colour = "Black", angle = 45, hjust = 1),
                                        legend.key.height = unit(1, "cm"),
                                        legend.key.width = unit(1, "cm"),
                                        legend.position = "none",
                                        legend.background = element_rect(size = 0.1, colour = "Black"),
                                        legend.key = element_blank(),
                                        legend.text = element_text(size = 20),
                                        legend.title = element_text(size = 20),
                                        strip.background = element_rect(
                                          fill="white"),
                                        axis.line = element_line()
      )
  )
  if(i == 5){
    grid.arrange(a, b, c, d, e)
  }
}



ggplot(split_domains(files[[1]]), aes(x = Nodes, y = Mean)) + geom_point(aes(color = NetworkDomain)) +
  geom_errorbar(aes(x = Nodes, ymin = min, ymax = max, color = NetworkDomain)) + scale_x_log10() +
  scale_y_log10(labels = function(x) format(x, scientific = FALSE)) + ylab("Number of Iterations") +
  scale_color_manual(values = c("gray20", "orangered1")) +
  facet_grid(NetworkDomain ~ .) + theme(panel.background = element_blank(),
                                        panel.grid.major = element_line(colour = "gray85", size = 1),
                                        panel.grid.minor = element_line(colour = "gray85"),
                                        axis.ticks = element_blank(),
                                        axis.title.y = element_text(size = 20),
                                        axis.title.x = element_text(size = 25),
                                        axis.text.x = element_text(size = 20, colour = "Black", angle = 45, hjust = 1),
                                        axis.text.y = element_text(size = 20, colour = "Black", angle = 45, hjust = 1),
                                        legend.key.height = unit(1, "cm"),
                                        legend.key.width = unit(1, "cm"),
                                        legend.position = "none",
                                        legend.background = element_rect(size = 0.1, colour = "Black"),
                                        legend.key = element_blank(),
                                        legend.text = element_text(size = 20),
                                        legend.title = element_text(size = 20),
                                        strip.background = element_rect(
                                          fill="white"),
                                        axis.line.x = element_line()
                                        )

