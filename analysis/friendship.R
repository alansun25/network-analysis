setwd("analysis/klas12b")

source("../../functions/random_graph.R")
source("../../functions/rewire_lattice.R")
source("../../functions/vboot.R")
source("../../functions/jackknife.R")

library(igraph)

f1 <- as.matrix(read.table("klas12b-net-1.dat"))
f2 <- as.matrix(read.table("klas12b-net-2.dat"))
f3 <- as.matrix(read.table("klas12b-net-3m.dat"))
f4 <- as.matrix(read.table("klas12b-net-4m.dat"))

f1[f1 == 9] <- NA
f2[f2 == 9] <- NA
f3[f3 == 9] <- NA
f4[f4 == 9] <- NA

g1 <- graph_from_adjacency_matrix(f1, mode = "undirected", add.colnames = NA)
g2 <- graph_from_adjacency_matrix(f2, mode = "undirected", add.colnames = NA)
g3 <- graph_from_adjacency_matrix(f3, mode = "undirected", add.colnames = NA)
g4 <- graph_from_adjacency_matrix(f4, mode = "undirected", add.colnames = NA)
