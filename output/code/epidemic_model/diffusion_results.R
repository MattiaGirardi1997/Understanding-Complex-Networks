######## install packages ########
list.of.packages <- c("data.table", "igraph", "jsonlite", "ggplot2", "gridExtra", "mlbench",
                      "caret", "e1071", "corrplot")
install.packages(list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])])
sapply(list.of.packages, library, character.only = TRUE)
rm(list.of.packages)

# load in measures data
master_data <- fread("output/master_measures.csv")

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
D_1_50_70 <- fread("removed_loops/diffusion/consolidated/1% starting_50% prob_70% threshold.csv")
D_1_50_70 <- D_1_50_70[D_1_50_70$Name %in% master_data$Name]
D_01_100_50 <- fread("removed_loops/diffusion/consolidated/0.1% starting_100% prob_50% threshold.csv")
D_01_100_50 <- D_01_100_50[D_01_100_50$Name %in% master_data$Name]
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
master_data$D_01_100_50 <- D_01_100_50$Mean
master_data$D_1_50_50 <- D_1_50_50$Mean
master_data$D_1_100_50 <- D_1_100_50$Mean
master_data$D_1_50_70 <- D_1_50_70$Mean
master_data$D_1_100_70 <- D_1_100_70$Mean
master_data$D_5_50_70 <- D_5_50_70$Mean
master_data$D_5_100_70 <- D_5_100_70$Mean

master_data <- split_domains(master_data[, c(3:4, 25:30)])
master_data <- na.omit(master_data)

# analyze spread of max and min
files <- lapply(list(D_01_100_50, D_1_50_50, D_1_100_50, D_1_50_70, D_1_100_70,
                     D_5_50_70, D_5_100_70), split_domains)

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

ggplot(files[[1]], aes(x = Nodes, y = Mean)) +
  geom_point(aes(color = NetworkDomain)) +
  scale_x_log10(breaks = c(100, 1000, 10000)) +
  scale_y_log10(labels = function(x) format(x, scientific = FALSE)) + ylab("Number of Iterations") +
  scale_color_manual(values = c("gray20", "orangered1")) + labs(title = sprintf("%s", specs[i])) +
  facet_wrap(Domain ~ ., nrow = 1) + theme(panel.background = element_blank(),
                                                             panel.grid.major = element_line(colour = "gray85", size = 1),
                                                             panel.grid.minor = element_line(colour = "gray85"),
                                                             axis.ticks = element_blank(),
                                                             axis.title.y = element_text(size = 20),
                                                             axis.title.x = element_text(size = 25),
                                                             axis.text.x = element_text(size = 20, colour = "Black", angle = 45, hjust = 1),
                                                             axis.text.y = element_text(size = 20, colour = "Black", angle = 45, hjust = 1),
                                                             legend.position = "none",
                                                             title = element_text(size = 20),
                                                             strip.background = element_rect(
                                                               fill="white"),
                                                             strip.text = element_text(size = 20, face = "bold"),
                                                             axis.line = element_line(),
                                                             axis.title.x.bottom = element_blank(),
                                                             plot.margin=unit(c(2,2,2,2),"cm"))


files <- lapply(list(D_1_50_50, D_1_100_50, D_1_50_70, D_1_100_70, D_5_50_70, D_5_100_70), na.omit)
files <- lapply(list(D_1_50_50, D_1_100_50, D_1_50_70, D_1_100_70, D_5_50_70, D_5_100_70), split_domains)

mean(apply(files[[1]][NetworkDomain == "Non-Social", 15], 1, mean), na.rm = T)
mean(apply(files[[2]][, 15], 1, mean), na.rm = T)
mean(apply(files[[3]][, 15], 1, mean), na.rm = T)
mean(apply(files[[4]][, 15], 1, mean), na.rm = T)
mean(apply(files[[5]][, 15], 1, mean), na.rm = T)
mean(apply(files[[6]][, 15], 1, mean), na.rm = T)











