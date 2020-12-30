webkb_webkb_cornell_cocite
net <- fread(sprintf("data/all_data/%s.csv", master[480, Name]))
unique(net)

master <- fread("output/undirected/master_measures_2.csv")
removed <- fread("removed_loops/output/removed_loops_table.csv")
master[480, Name]

k <- 1
net <- fread(sprintf("data/all_data/%s.csv", master[k, Name]))
name <- master[k, Name]
net <- unique(net)
net$ID <- 1:nrow(net)
final <- net
for(i in final$ID){
  if(net[i, Node1] %in% net$Node2){
    c <- which(net$Node2 %in% net[i, Node1])
    for(j in c){
      if(net[j, Node1] == net[i, Node2]){
        final <- final[!ID == j]
      }
    }
  }
}



master <- fread("output/undirected/master_measures_2.csv")
k <- 1
net <- fread(sprintf("data/all_data/%s.csv", master[k, Name]))
name <- master[k, Name]
net <- unique(net)
net$ID <- 1:nrow(net)
final <- net
loops <- c()
i <- 1
while(i < nrow(net)){
  if(net[i, Node1] %in% net$Node2 & !i %in% loops){
    c <- which(net$Node2 %in% net[i, Node1])
    for(j in c){
      i <- 1
      j <- 15
      if(){
        final <- final[!ID == j]
        loops[length(loops) + 1] <- j
      }
      i <- i + 1
    }
  }
}



master <- fread("output/undirected/master_measures_2.csv")
for(k in 1:1){
  name <- master[k, Name]
  net <- unique(fread(sprintf("data/all_data/%s.csv", master[k, Name])))
  el <- net
  el <- unique(el)
  el$ID <- 1:nrow(el)
  loops <- c()
  for(i in 1:100){
    c <- which(el$Node2 %in% el[i, Node1])
    
    if(length(el[c, ID][which(el[c, Node1] %in% el[i, Node2])]) != 0){
      loops[length(loops) + 1] <- el[c, ID][which(el[c, Node1] %in% el[i, Node2])]
      el <- el[!ID == el[c, ID][which(el[c, Node1] %in% el[i, Node2])]]
    }
  }
  loop_el <- net[loops]
  loop_el <- data.table(Node1 = loop_el$Node2, Node2 = loop_el$Node1)
  net <- net[-unique(loops)]
  final <- unique(rbind(net, loop_el))
  write.table(final, file = sprintf("new/%s.csv", name), row.names = F, sep = ",")
}



net <- unique(fread(sprintf("data/all_data/%s.csv", master[k, Name])))
loop <- net[loops]
net <- net[-unique(loops)]
loop <- data.table(Node1 = loop$Node2, Node2 = loop$Node1)
new <- rbind(net, loop)
fin <- unique(new)


master <- fread("output/undirected/master_measures_2.csv")
for(k in 1:1){
  k <- 1
  name <- master[k, Name]
  net <- unique(fread(sprintf("data/all_data/%s.csv", master[k, Name])))
  el <- net
  nodes <- unique(c(el$Node1, el$Node2))
  for(n in nodes){
    
  }
}



master <- fread("output/undirected/master_measures_2.csv")
for(k in 1:513){
  name <- master[k, Name]
  el <- unique(fread(sprintf("data/all_data/%s.csv", master[k, Name])))
  el <- el[order(el$Node1)]
  el$ID <- 1:nrow(el)
  loops <- c()
  for(i in 1:nrow(el)){
    c <- which(el$Node2 %in% el[i, Node1])
    
    if(length(el[c, ID][which(el[c, Node1] %in% el[i, Node2])]) != 0){
      loops[length(loops) + 1] <- el[c, ID][which(el[c, Node1] %in% el[i, Node2])]
      el <- el[!ID == el[c, ID][which(el[c, Node1] %in% el[i, Node2])]]
    }
  }
  write.table(el[, 1:2], file = sprintf("new/%s.csv", name), row.names = F, sep = ",")
}


  node1 <- el[Node1 == 1]
node2 <- el[Node2 == 1]
node2 <- data.table(Node1 = node2$Node2, Node2 = node2$Node1)
g <- rbind(node1, node2)
g <- unique(g)

length(el[Node1 == 1])
length(el[Node2 == 1])

k <- 1
name <- master[k, Name]

real <- fread("new/1.csv")
real$ID <- 1:13
net <- fread("new/2.csv")
el <- net
el <- unique(el)
el$ID <- 1:nrow(el)
el2 <- el
loops <- c()
for(i in 1:100){
  c <- which(el$Node2 %in% el[i, Node1])
  if(length(el[c, ID][which(el[c, Node1] %in% el[i, Node2])]) != 0){
    loops[length(loops) + 1] <- el[c, ID][which(el[c, Node1] %in% el[i, Node2])]
    el <- el[!ID == el[c, ID][which(el[c, Node1] %in% el[i, Node2])]]
  }
}






sort(el2$Node1)

el2 <- el2[order(el2[,1], el2[, 2])]


 

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  