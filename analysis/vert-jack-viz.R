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

mdjack_f1 <- jackknife(fg1, "meandegree")
mdjack_f2 <- jackknife(fg2, "meandegree")
mdjack_f3 <- jackknife(fg3, "meandegree")
mdjack_f4 <- jackknife(fg4, "meandegree")

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

tboot_f1 <- sapply(numeric(100), function(x) x + transitivity(vboot(fg1, v)))
tboot_f2 <- sapply(numeric(100), function(x) x + transitivity(vboot(fg2, v)))
tboot_f3 <- sapply(numeric(100), function(x) x + transitivity(vboot(fg3, v)))
tboot_f4 <- sapply(numeric(100), function(x) x + transitivity(vboot(fg4, v)))

mdboot_f1 <- sapply(numeric(100), function(x) x + mean(degree(vboot(fg1, v))))
mdboot_f2 <- sapply(numeric(100), function(x) x + mean(degree(vboot(fg2, v))))
mdboot_f3 <- sapply(numeric(100), function(x) x + mean(degree(vboot(fg3, v))))
mdboot_f4 <- sapply(numeric(100), function(x) x + mean(degree(vboot(fg4, v))))

dboot_f1 <- sapply(numeric(100), function(x) x + edge_density(vboot(fg1, v)))
dboot_f2 <- sapply(numeric(100), function(x) x + edge_density(vboot(fg2, v)))
dboot_f3 <- sapply(numeric(100), function(x) x + edge_density(vboot(fg3, v)))
dboot_f4 <- sapply(numeric(100), function(x) x + edge_density(vboot(fg4, v)))

kboot_f1 <- sapply(numeric(100), function(x) x + sum(coreness(vboot(fg1, v)) > 1))
kboot_f2 <- sapply(numeric(100), function(x) x + sum(coreness(vboot(fg2, v)) > 1))
kboot_f3 <- sapply(numeric(100), function(x) x + sum(coreness(vboot(fg3, v)) > 1))
kboot_f4 <- sapply(numeric(100), function(x) x + sum(coreness(vboot(fg4, v)) > 1))

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

tjack_tg97 <- jackknife(tg97, "transitivity")
tjack_tg98 <- jackknife(tg98, "transitivity")
tjack_tg99 <- jackknife(tg99, "transitivity")

mdjack_tg97 <- jackknife(tg97, "meandegree")
mdjack_tg98 <- jackknife(tg98, "meandegree")
mdjack_tg99 <- jackknife(tg99, "meandegree")

djack_tg97 <- jackknife(tg97, "density")
djack_tg98 <- jackknife(tg98, "density")
djack_tg99 <- jackknife(tg99, "density")

kjack_tg97 <- jackknife(tg97, "corenessnum")
kjack_tg98 <- jackknife(tg98, "corenessnum")
kjack_tg99 <- jackknife(tg99, "corenessnum")

# Create bootstrap distributions of transitivity, mean degree, 
# density, and number of nodes in a k-core where k > 1.
set.seed(346)

v <- vertices(sample(V(tg97), gorder(tg97), replace=TRUE))

tboot_tg97 <- sapply(numeric(100), function(x) x + transitivity(vboot(tg97, v)))
tboot_tg98 <- sapply(numeric(100), function(x) x + transitivity(vboot(tg98, v)))
tboot_tg99 <- sapply(numeric(100), function(x) x + transitivity(vboot(tg99, v)))

mdboot_tg97 <- sapply(numeric(100), function(x) x + mean(degree(vboot(tg97, v))))
mdboot_tg98 <- sapply(numeric(100), function(x) x + mean(degree(vboot(tg98, v))))
mdboot_tg99 <- sapply(numeric(100), function(x) x + mean(degree(vboot(tg99, v))))

dboot_tg97 <- sapply(numeric(100), function(x) x + edge_density(vboot(tg97, v)))
dboot_tg98 <- sapply(numeric(100), function(x) x + edge_density(vboot(tg98, v)))
dboot_tg99 <- sapply(numeric(100), function(x) x + edge_density(vboot(tg99, v)))

kboot_tg97 <- sapply(numeric(100), function(x) x + sum(coreness(vboot(tg97, v)) > 1))
kboot_tg98 <- sapply(numeric(100), function(x) x + sum(coreness(vboot(tg98, v)) > 1))
kboot_tg99 <- sapply(numeric(100), function(x) x + sum(coreness(vboot(tg99, v)) > 1))
