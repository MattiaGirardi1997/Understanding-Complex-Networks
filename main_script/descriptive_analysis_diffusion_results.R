
res_10_100_70 <- fread("output/diffusion/consolidated/10% starting_100% prob_70% threshold.csv")[, c(1:4, 15)]
res_10_50_70 <- fread("output/diffusion/consolidated/10% starting_50% prob_70% threshold.csv")[, c(1:4, 15)]
res_5_100_70 <- fread("output/diffusion/consolidated/5% starting_100% prob_70% threshold.csv")[, c(1:4, 15)]
res_5_50_50 <- fread("output/diffusion/consolidated/5% starting_50% prob_50% threshold.csv")[, c(1:4, 15)]
res_5_50_70 <- fread("output/diffusion/consolidated/5% starting_50% prob_70% threshold.csv")[, c(1:4, 15)]
res_5_75_70 <- fread("output/diffusion/consolidated/5% starting_75% prob_70% threshold.csv")[, c(1:4, 15)]

g <- data.table(res_10_100_70[,1:4], "10_100_70" = res_10_100_70$Mean, "10_50_70" = res_10_50_70$Mean, 
                "5_100_70" = res_5_100_70$Mean, 
                "5_50_50" = res_5_50_50$Mean,
                "5_50_70" = res_5_50_70$Mean, 
                "5_75_70" = res_5_75_70$Mean)

data_long <- tidyr::gather(g, key = type_col, value = categories, -c("Name", "Domain", "Nodes", "Edges"))
data_long <- na.omit(data_long)
data_long$ratio1 <- data_long$categories/data_long$Nodes
data_long$ratio2 <- data_long$Nodes/data_long$categories
ggplot(data_long, aes(x = Nodes, y = categories, color = type_col)) + geom_line(size = 1)
ggplot(data_long, aes(x = Nodes, y = ratio1, color = type_col)) + geom_line(size = 1)
ggplot(data_long, aes(x = Nodes, y = ratio2, color = type_col)) + geom_line(size = 1)


