removed <- fread("output/diffusion/consolidated/consolidated_removed/10% starting_100% prob_70% threshold.csv")
normal <- fread("output/diffusion/consolidated/10% starting_100% prob_70% threshold.csv")

ggplot() +
  geom_line(data = removed, aes(x = Nodes, y = Mean), color = "red") +
  geom_line(data = normal, aes(x = Nodes, y = Mean), color = "blue")


g <- data.table(removed[, Name], removed[, Nodes], removed[, Domain], Mean_removed = removed[, "Mean"],
                Mean = normal[, "Mean"], Diff = normal$Mean-removed$Mean)

ggplot() +
  geom_line(data = g, aes(x = Nodes, y = Diff), color = "red")
