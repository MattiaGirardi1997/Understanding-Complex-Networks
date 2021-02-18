### create text files for computation
master <- fread("removed_loops/output/master_measures_removed_loops.csv")
for(i in 1:nrow(master)){
  name <- master[i, Name]
  net <- fread(sprintf("removed_loops/data/%s.csv", name))
  write.table(net, file = sprintf("removed_loops/txt/%s.txt", name), row.names = F, sep = " ", col.names = F)
}

### Including entropy and complexity measures
measures <- fread("removed_loops/output/master_measures_removed_loops.csv")

complexity_and_entropy_measures <- fread("removed_loops/output/complexity_and_entropy_measures.csv")[, -1]

complexity_and_entropy_measures$Name <- gsub(".txt", "", complexity_and_entropy_measures$Name)
complexity_and_entropy_measures <- complexity_and_entropy_measures[order(complexity_and_entropy_measures$Name)]

complexity_and_entropy_measures <- complexity_and_entropy_measures[Name %in% measures$Name]

master_measures <- data.table(measures[,1:13], Complexity = complexity_and_entropy_measures$Complexity,
                              Entropy = complexity_and_entropy_measures$Entropy,
                              measures[,14:21])

write.table(master_measures, file = "removed_loops/output/master_measures_removed_loops.csv", sep = ",", row.names = F)


