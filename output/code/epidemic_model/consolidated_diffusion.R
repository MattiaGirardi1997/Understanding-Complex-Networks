library(data.table)

# calculate mean of diffusion
files <- list.files(path = "data/diffusion", pattern="*.csv", full.names=TRUE)[11:20]
names <- list.files(path = "removed_loops/diffusion", pattern="*.csv", full.names=FALSE)[11:20]
files

for(i in 1:length(files)){
  if(i == 1){
    res_table <- fread(files[i])
    names(res_table)[5] <- c("Iteration_1")
    } else {
    res <- fread(files[i])[, sprintf("Iteration_%s", i) := Iterations][, 6]
    res_table <- cbind(res_table, res)
    }
}

name <- names[1]
name <- gsub("_1.csv", "", name)
res_table <- data.table(res_table[, 1:4], sapply(res_table[, 5:ncol(res_table)], as.integer))
res_table <- res_table[, Mean := rowMeans(res_table[, 5:ncol(res_table)])]
write.table(res_table, file = sprintf("removed_loops/diffusion/consolidated/%s.csv", name), row.names = F,
              sep = ",")
names <- names[-c(1:10)]
files <- files[-c(1:10)]

##### consolidate rerun diffusion results
files <- list.files(path = "removed_loops/diffusion/rerun", pattern="rerun_", full.names=TRUE)[11:20]
names <- list.files(path = "removed_loops/diffusion/rerun", pattern="*rerun_", full.names=FALSE)[11:20]
files

for(i in 1:length(files)){
  if(i == 1){
    res_table <- fread(files[i])
    names(res_table) <- c("Name", "NetworkDomain", "Nodes", "Edges", "Iteration_1")
  } else {
    res <- fread(files[i])
    names(res) <- c("Name", "NetworkDomain", "Nodes", "Edges", "Iteration_1")
    res <- res[, sprintf("Iteration_%s", i) := Iteration_1][, 6]
    res_table <- cbind(res_table, res)
  }
}
name <- names[1]
name <- gsub("_1.csv", "", name)
res_table <- data.table(res_table[, 1:4], sapply(res_table[, 5:ncol(res_table)], as.integer))
res_table <- res_table[, Mean := rowMeans(res_table[, 5:ncol(res_table)])]
write.table(res_table, file = sprintf("removed_loops/diffusion/consolidated/%s.csv", name), row.names = F,
            sep = ",")
names <- names[-c(1:10)]
files <- files[-c(1:10)]

### add rerun diffusion results
master_data <- fread("removed_loops/output/master_measures_removed_loops.csv")[-c(103:111)]


# D_01_100_50
rerun <- fread("removed_loops/diffusion/consolidated/rerun_0.1% starting_100% prob_50% threshold.csv")
D_01_100_50 <- fread("removed_loops/diffusion/consolidated/0.1% starting_100% prob_50% threshold.csv")

D_01_100_50 <- D_01_100_50[!which(D_01_100_50$Name %in% rerun$Name)]
D_01_100_50 <- rbind(D_01_100_50, rerun)
D_01_100_50  <- D_01_100_50[order(D_01_100_50$Name)]
write.table(D_01_100_50, file = "removed_loops/diffusion/consolidated/0.1% starting_100% prob_50% threshold.csv", row.names = F,
            sep = ",")

# D_1_50_70
rerun <- fread("removed_loops/diffusion/consolidated/rerun_1% starting_50% prob_70% threshold.csv")
D_1_50_70 <- fread("removed_loops/diffusion/consolidated/1% starting_50% prob_70% threshold.csv")

D_1_50_70 <- D_1_50_70[!which(D_1_50_70$Name %in% rerun$Name)]
D_1_50_70 <- rbind(D_1_50_70, rerun)
D_1_50_70  <- D_1_50_70[order(D_1_50_70$Name)]
write.table(D_1_50_70, file = "removed_loops/diffusion/consolidated/1% starting_50% prob_70% threshold.csv",
            row.names = F, sep = ",")
# D_1_50_50
rerun <- fread("removed_loops/diffusion/consolidated/rerun_1% starting_50% prob_50% threshold.csv")
D_1_50_50 <- fread("removed_loops/diffusion/consolidated/1% starting_50% prob_50% threshold.csv")

D_1_50_50 <- D_1_50_50[!which(D_1_50_50$Name %in% rerun$Name)]
D_1_50_50 <- rbind(D_1_50_50, rerun)
D_1_50_50  <- D_1_50_50[order(D_1_50_50$Name)]
D_1_50_50 <- D_1_50_50[D_1_50_50$Name %in% master_data$Name]


write.table(D_1_50_50, file = "removed_loops/diffusion/consolidated/1% starting_50% prob_50% threshold.csv",
            row.names = F, sep = ",")

# D_1_100_50
rerun <- fread("removed_loops/diffusion/consolidated/rerun_1% starting_100% prob_50% threshold.csv")
D_1_100_50 <- fread("removed_loops/diffusion/consolidated/1% starting_100% prob_50% threshold.csv")

D_1_100_50 <- D_1_100_50[!which(D_1_100_50$Name %in% rerun$Name)]
D_1_100_50 <- rbind(D_1_100_50, rerun)
D_1_100_50  <- D_1_100_50[order(D_1_100_50$Name)]
D_1_100_50 <- D_1_100_50[D_1_100_50$Name %in% master_data$Name]

write.table(D_1_100_50, file = "removed_loops/diffusion/consolidated/1% starting_100% prob_50% threshold.csv",
            row.names = F, sep = ",")

# D_1_100_70
rerun <- fread("removed_loops/diffusion/consolidated/rerun_1% starting_100% prob_70% threshold.csv")
D_1_100_70 <- fread("removed_loops/diffusion/consolidated/1% starting_100% prob_70% threshold.csv")

D_1_100_70 <- D_1_100_70[!which(D_1_100_70$Name %in% rerun$Name)]
D_1_100_70 <- rbind(D_1_100_70, rerun)
D_1_100_70  <- D_1_100_70[order(D_1_100_70$Name)]
D_1_100_70 <- D_1_100_70[D_1_100_70$Name %in% master_data$Name]

write.table(D_1_100_70, file = "removed_loops/diffusion/consolidated/1% starting_100% prob_70% threshold.csv",
            row.names = F, sep = ",")

# D_5_50_70
rerun <- fread("removed_loops/diffusion/consolidated/rerun_5% starting_50% prob_70% threshold.csv")
D_5_50_70 <- fread("removed_loops/diffusion/consolidated/5% starting_50% prob_70% threshold.csv")

D_5_50_70 <- D_5_50_70[!which(D_5_50_70$Name %in% rerun$Name)]
D_5_50_70 <- rbind(D_5_50_70, rerun)
D_5_50_70  <- D_5_50_70[order(D_5_50_70$Name)]
D_5_50_70 <- D_5_50_70[D_5_50_70$Name %in% master_data$Name]

write.table(D_5_50_70, file = "removed_loops/diffusion/consolidated/5% starting_50% prob_70% threshold.csv",
            row.names = F, sep = ",")

# D_5_100_70
rerun <- fread("removed_loops/diffusion/consolidated/rerun_5% starting_100% prob_70% threshold.csv")
D_5_100_70 <- fread("removed_loops/diffusion/consolidated/5% starting_100% prob_70% threshold.csv")

D_5_100_70 <- D_5_100_70[!which(D_5_100_70$Name %in% rerun$Name)]
D_5_100_70 <- rbind(D_5_100_70, rerun)
D_5_100_70  <- D_5_100_70[order(D_5_100_70$Name)]
D_5_100_70 <- D_5_100_70[D_5_100_70$Name %in% master_data$Name]

write.table(D_5_100_70, file = "removed_loops/diffusion/consolidated/5% starting_100% prob_70% threshold.csv",
            row.names = F, sep = ",")

### creating diffusion results table
# load in measure table
master_data <- fread("removed_loops/output/master_measures_removed_loops.csv")

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

master_data <- na.omit(master_data)
master_data[, 4:length(master_data)] <- data.table(apply(master_data[, 4:length(master_data)],
                                                         2, scale))

write.table(master_data, file = "removed_loops/output/master_diffusion.csv", row.names = F,
            sep = ",")


