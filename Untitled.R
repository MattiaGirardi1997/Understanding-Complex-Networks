ICON.files <- as.vector(ICON_data$Name)
OLP_data <- as.data.frame(fread("input/import_datasets/OLP_networks.csv"))
OLP.files <- as.vector(OLP_data$title)
netzschleuder.files <- as.vector


for (i in 1:length(OLP.files)){
  g <- agrep(OLP.files[i], ICON.files, value=T, ignore.case = T)
  print(g)
}

rm(g, i)

OLP_data <- as.data.frame(fread("input/import_datasets/OLP_networks.csv"))

agrep(ICON.files[1], OLP.files, value=T, ignore.case = T)

