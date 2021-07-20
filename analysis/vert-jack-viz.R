library(igraph)

setwd("/Users/alansun/github/network-analysis/analysis")

source("../functions/vboot.R")
source("../functions/jackknife.R")

# Read the friendship network data
f1 <- as.matrix(read.table("friendship/klas12b/klas12b-net-1.dat"))
f2 <- as.matrix(read.table("friendship/klas12b/klas12b-net-2.dat"))
f3 <- as.matrix(read.table("friendship/klas12b/klas12b-net-3m.dat"))
f4 <- as.matrix(read.table("friendship/klas12b/klas12b-net-4m.dat"))

# Remove missing data points
f1[f1 == 9] <- NA
f2[f2 == 9] <- NA
f3[f3 == 9] <- NA
f4[f4 == 9] <- NA

# Create graphs for each network
fg1 <- graph_from_adjacency_matrix(f1, mode = "undirected", add.colnames = NA)
fg2 <- graph_from_adjacency_matrix(f2, mode = "undirected", add.colnames = NA)
fg3 <- graph_from_adjacency_matrix(f3, mode = "undirected", add.colnames = NA)
fg4 <- graph_from_adjacency_matrix(f4, mode = "undirected", add.colnames = NA)

# Create jackknife distributions of transitivity, mean degree, 
# density, and number of nodes in a k-core where k > 1.
set.seed(346)

tjack_f1 <- jackknife(fg1, "transitivity")
tjack_f2 <- jackknife(fg2, "transitivity")
tjack_f3 <- jackknife(fg3, "transitivity")
tjack_f4 <- jackknife(fg4, "transitivity")

mjack_f1 <- jackknife(fg1, "meandegree")
mjack_f2 <- jackknife(fg2, "meandegree")
mjack_f3 <- jackknife(fg3, "meandegree")
mjack_f4 <- jackknife(fg4, "meandegree")

djack_f1 <- jackknife(fg1, "density")
djack_f2 <- jackknife(fg2, "density")
djack_f3 <- jackknife(fg3, "density")
djack_f4 <- jackknife(fg4, "density")

kjack_f1 <- jackknife(fg1, "corenessnum")
kjack_f2 <- jackknife(fg2, "corenessnum")
kjack_f3 <- jackknife(fg3, "corenessnum")
kjack_f4 <- jackknife(fg4, "corenessnum")

# Create bootstrap distributions of transitivity, mean degree, 
# density, and number of nodes in a k-core where k > 1.
set.seed(346)

# Only sampling nodes from fg1 since all four networks have the same nodes
v <- vertices(sample(V(fg1), gorder(fg1), replace=TRUE))

tboot_f1 <- mboot_f1 <- dboot_f1 <- kboot_f1 <- 
  tboot_f2 <- mboot_f2 <- dboot_f2 <- kboot_f2 <- 
  tboot_f3 <- mboot_f3 <- dboot_f3 <- kboot_f3 <-
  tboot_f4 <- mboot_f4 <- dboot_f4 <- kboot_f4 <- c()

for(i in 1:1000) {
  b1 <- vboot(fg1, v)
  b2 <- vboot(fg2, v)
  b3 <- vboot(fg3, v)
  b4 <- vboot(fg4, v)
  
  tboot_f1 <- c(tboot_f1, transitivity(b1))
  mboot_f1 <- c(mboot_f1, mean(degree(b1)))
  dboot_f1 <- c(dboot_f1, edge_density(b1))
  kboot_f1 <- c(kboot_f1, sum(coreness(b1) > 1))
  
  tboot_f2 <- c(tboot_f2, transitivity(b2))
  mboot_f2 <- c(mboot_f2, mean(degree(b2)))
  dboot_f2 <- c(dboot_f2, edge_density(b2))
  kboot_f2 <- c(kboot_f2, sum(coreness(b2) > 1))
  
  tboot_f3 <- c(tboot_f3, transitivity(b3))
  mboot_f3 <- c(mboot_f3, mean(degree(b3)))
  dboot_f3 <- c(dboot_f3, edge_density(b3))
  kboot_f3 <- c(kboot_f3, sum(coreness(b3) > 1))
  
  tboot_f4 <- c(tboot_f4, transitivity(b4))
  mboot_f4 <- c(mboot_f4, mean(degree(b4)))
  dboot_f4 <- c(dboot_f4, edge_density(b4))
  kboot_f4 <- c(kboot_f4, sum(coreness(b4) > 1))
}

# ================================================================================

# Read the tortoise interactions network data
tortoise <- read.table("tortoise/reptilia-tortoise-network-bsv.edges")

# Remove data points from 1996 (there are too few)
tortoise <- tortoise[which(tortoise[,3] != 1996),]

# Split the tortoise interaction data by year and remove the year column to obtain an edgelist
tort1997 <- tortoise[which(tortoise[,3] == 1997), -3]
tort1998 <- tortoise[which(tortoise[,3] == 1998), -3]
tort1999 <- tortoise[which(tortoise[,3] == 1999), -3]

# Create graphs
tg97 <- graph_from_edgelist(as.matrix(tort1997), directed = FALSE)
tg98 <- graph_from_edgelist(as.matrix(tort1998), directed = FALSE)
tg99 <- graph_from_edgelist(as.matrix(tort1999), directed = FALSE)

# Restrict the graphs to contain only the nodes they all have in common
common_nodes <- intersect(V(tg97), intersect(V(tg98), V(tg99)))
tg97 <- induced_subgraph(tg97, common_nodes, "auto")
tg98 <- induced_subgraph(tg98, common_nodes, "auto")
tg99 <- induced_subgraph(tg99, common_nodes, "auto")

# Create jackknife distributions of transitivity, mean degree, 
# density, and number of nodes in a k-core where k > 1.
set.seed(346)

tjack_t97 <- jackknife(tg97, "transitivity")
tjack_t98 <- jackknife(tg98, "transitivity")
tjack_t99 <- jackknife(tg99, "transitivity")

mjack_t97 <- jackknife(tg97, "meandegree")
mjack_t98 <- jackknife(tg98, "meandegree")
mjack_t99 <- jackknife(tg99, "meandegree")

djack_t97 <- jackknife(tg97, "density")
djack_t98 <- jackknife(tg98, "density")
djack_t99 <- jackknife(tg99, "density")

kjack_t97 <- jackknife(tg97, "corenessnum")
kjack_t98 <- jackknife(tg98, "corenessnum")
kjack_t99 <- jackknife(tg99, "corenessnum")

# Create bootstrap distributions of transitivity, mean degree, 
# density, and number of nodes in a k-core where k > 1.
set.seed(346)

v <- vertices(sample(V(tg97), gorder(tg97), replace=TRUE))
tboot_t97 <- tboot_t98 <- tboot_t99 <-
  mboot_t97 <- mboot_t98 <- mboot_t99 <-
  dboot_t97 <- dboot_t98 <- dboot_t99 <-
  kboot_t97 <- kboot_t98 <- kboot_t99 <- c()

for(i in 1:1000) {
  b1 <- vboot(tg97, v)
  b2 <- vboot(tg98, v)
  b3 <- vboot(tg99, v)
  
  tboot_t97 <- c(tboot_t97, transitivity(b1))
  mboot_t97 <- c(mboot_t97, mean(degree(b1)))
  dboot_t97 <- c(dboot_t97, edge_density(b1))
  kboot_t97 <- c(kboot_t97, sum(coreness(b1) > 1))
  
  tboot_t98 <- c(tboot_t98, transitivity(b2))
  mboot_t98 <- c(mboot_t98, mean(degree(b2)))
  dboot_t98 <- c(dboot_t98, edge_density(b2))
  kboot_t98 <- c(kboot_t98, sum(coreness(b2) > 1))
  
  tboot_t99 <- c(tboot_t99, transitivity(b3))
  mboot_t99 <- c(mboot_t99, mean(degree(b3)))
  dboot_t99 <- c(dboot_t99, edge_density(b3))
  kboot_t99 <- c(kboot_t99, sum(coreness(b3) > 1))
}

save.image(file = "env.RData")
