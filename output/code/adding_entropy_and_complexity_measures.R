### Including entropy and complexity measures

complexity_and_entropy_measures <- fread("output/complexity_and_entropy_measures.csv")[, -1]

complexity_and_entropy_measures$Name <- gsub(".txt", "", complexity_and_entropy_measures$Name)
complexity_and_entropy_measures <- complexity_and_entropy_measures[order(complexity_and_entropy_measures$Name)]

complexity_and_entropy_measures <- complexity_and_entropy_measures[Name %in% measures$Name]

master_measures_2 <- data.table(measures[,1:5], AverageComplexity = complexity_and_entropy_measures$AverageComplexity, 
                                measures[,6:9], Complexity = complexity_and_entropy_measures$Complexity, measures[,10:13], 
                                Entropy = complexity_and_entropy_measures$Entropy, measures[,14])

write.table(master_measures_2, file = "output/undirected/master_measures_2.csv", sep = ",", row.names = F)

