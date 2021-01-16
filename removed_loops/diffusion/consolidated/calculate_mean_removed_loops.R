library(data.table)

# calculate mean of diffusion
files <- list.files(path = "removed_loops/diffusion", pattern="*.csv", full.names=TRUE)
names <- list.files(path = "removed_loops/diffusion", pattern="*.csv", full.names=FALSE)
files
for(i in 1:10){
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
res_table <- data.table(res_table[, 1:4], sapply(res_table[, 5:14], as.integer))
res_table <- res_table[, Mean := rowMeans(res_table[, 5:14])]
write.table(res_table, file = sprintf("removed_loops/diffusion/consolidated/%s.csv", name), row.names = F,
              sep = ",")
names <- names[-c(1:10)]
files <- files[-c(1:10)]

