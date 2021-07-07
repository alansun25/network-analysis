setwd("analysis/klas12b")

# Import functions
source("../../functions/random_graph.R")
source("../../functions/rewire_lattice.R")
source("../../functions/vboot.R")
source("../../functions/jackknife.R")

library(igraph)

# Read each friendship network data set
f1 <- as.matrix(read.table("klas12b-net-1.dat"))
f2 <- as.matrix(read.table("klas12b-net-2.dat"))
f3 <- as.matrix(read.table("klas12b-net-3m.dat"))
f4 <- as.matrix(read.table("klas12b-net-4m.dat"))

# Remove missing data points
f1[f1 == 9] <- NA
f2[f2 == 9] <- NA
f3[f3 == 9] <- NA
f4[f4 == 9] <- NA

# Create graphs for each network
g1 <- graph_from_adjacency_matrix(f1, mode = "undirected", add.colnames = NA)
g2 <- graph_from_adjacency_matrix(f2, mode = "undirected", add.colnames = NA)
g3 <- graph_from_adjacency_matrix(f3, mode = "undirected", add.colnames = NA)
g4 <- graph_from_adjacency_matrix(f4, mode = "undirected", add.colnames = NA)

# ================================================================================
# Simulate draws of random graphs constrained to have the required degree sequence 
# from the friendship networks and get a distribution of transitivity
# ================================================================================

# Friendship network 1
set.seed(346)
tr1 <- c()
g1_degs <- degree(g1)
for(i in 1:1000) {
  rgds <- sample_degseq(g1_degs, method="vl")
  rt <- transitivity(rgds, type = "global")
  tr1 <- append(tr1, rt)
}

hist(tr1, main = "Distribution of Transitivity (Required Degree Sequence from f1)",
     xlab = "Transitivity Value", col = "lightblue")
summary(tr1) # Min: 0.1569, Q1: 0.2123, Med: 0.24, Mean: 0.2382, Q3: 0.2585, Max: 0.3415
transitivity(g1, type = "global") # Observed transitivity = 0.5076923

# Friendship network 2 (3 months after network 1)
tr2 <- c()
g2_degs <- degree(g2)
for(i in 1:1000) {
  rgds <- sample_degseq(g2_degs, method="vl")
  rt <- transitivity(rgds, type = "global")
  tr2 <- append(tr2, rt)
}

hist(tr2, main = "Distribution of Transitivity (Required Degree Sequence from f2)",
     xlab = "Transitivity Value", col = "lavender")
summary(tr2) # Min: 0.2780, Q1: 0.3355, Med: 0.3498, Mean: 0.3512, Q3: 0.3642, Max: 0.4217
transitivity(g2, type = "global") # Observed transitivity = 0.5609756

