
# calculate mean of diffusion
files <- list.files(path = "output/diffusion", pattern="*.csv", full.names=TRUE)[1:60]
names <- list.files(path = "output/diffusion", pattern="*.csv", full.names=FALSE)[1:60]
files
for(n in 1:6){
  for(i in 1:10){
    if(i == 1){
      res_table <- fread(files[i])
    } else {
      res <- fread(files[i])[, sprintf("Iteration_%s", i) := Iterations_1][, 6]
      res_table <- cbind(res_table, res)
    }
  }
  name <- names[1]
  name <- gsub("_1.csv", "", name)
  res_table <- data.table(res_table[, 1:4], sapply(res_table[, 5:14], as.numeric))
  res_table <- res_table[, Mean := rowMeans(res_table[, 5:14])]
  write.table(res_table, file = sprintf("output/diffusion/consolidated/%s.csv", name), row.names = F,
              sep = ",")
  names <- names[-c(1:10)]
  files <- files[-c(1:10)]
}

# calculate mean of removed diffusion
files <- list.files(path = "output/diffusion/removed", pattern="*.csv", full.names=TRUE)
names <- list.files(path = "output/diffusion/removed", pattern="*.csv", full.names=FALSE)
files
for(n in 1:6){
  for(i in 1:10){
    if(i == 1){
      res_table <- fread(files[i])
    } else {
      res <- fread(files[i])[, sprintf("Iteration_%s", i) := Iterations_1][, 6]
      res_table <- cbind(res_table, res)
    }
  }
  name <- names[1]
  name <- gsub("_1.csv", "", name)
  res_table <- data.table(res_table[, 1:4], sapply(res_table[, 5:14], as.numeric))
  res_table <- res_table[, Mean := rowMeans(res_table[, 5:14])]
  write.table(res_table, file = sprintf("output/diffusion/consolidated/consolidated_removed/%s.csv", name), row.names = F,
              sep = ",")
  names <- names[-c(1:10)]
  files <- files[-c(1:10)]
}

# calculate mean of random diffusion
files <- list.files(path = "output/diffusion/random", pattern="*.csv", full.names=TRUE)
names <- list.files(path = "output/diffusion/random", pattern="*.csv", full.names=FALSE)
files
for(i in 1:4){
    if(i == 1){
      res_table <- fread(files[i])
    } else {
      res <- fread(files[i])[, sprintf("Iteration_%s", i) := Iterations][, 6]
      res_table <- cbind(res_table, res)
    }
}
name <- names[1]
name <- gsub("_1.csv", "", name)
res_table <- data.table(res_table[, 1:4], sapply(res_table[, 5:8], as.numeric))
res_table <- res_table[, Mean := rowMeans(res_table[, 5:8])]
write.table(res_table, file = sprintf("output/diffusion/consolidated/consolidated_random/%s.csv", name), row.names = F,
            sep = ",")
