setwd("analysis/friendship/klas12b")

# Import functions
source("../../../functions/random_graph.R")
source("../../../functions/rewire_lattice.R")
source("../../../functions/vboot.R")
source("../../../functions/jackknife.R")
source("../../../functions/numcores.R")

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

# ============================================================================

# Simulate draws of random graphs using different methods and see where the
# observed transivities fall on the generated distributions

## Friendship network 1
transitivity(g1, type = "global") # 0.5076923
mean_distance(g1) # 2.84

### Erdos-Renyi
set.seed(346)
er1 <- c()
for(i in 1:1000) {
  # g1 has 63 edges and 26 nodes; set p = 0.1938461538 for r to have an expected edge count of 63
  # 0.1938461538 * 26(25)/2 = 63
  r <- random_graph(gorder(g1), 0.1938461538)
  rt <- transitivity(r, type = "global")
  er1 <- append(er1, rt)
}
hist(er1, main = "Distribution of Transitivity (Erdos-Renyi, f1)",
     xlab = "Transitivity Value", col = "lightblue")
summary(er1) # Min: 0.03409, Q1: 0.16116, Med: 0.18853, Mean: 0.18855, Q3: 0.21602, Max: 0.32308

### Small World
steps <- seq(-4, -0.5, 0.1)
len <- length(steps)
tr <- numeric(len)
md <- numeric(len)
for(i in 1:len) {
  trtemp <- numeric(10)
  mdtemp <- numeric(10)
  for(j in 1:10) {
    g <- rewire_lattice(gorder(g1)*15, 2, 10^steps[i])
    trtemp[j] <- transitivity(g)
    mdtemp[j] <- mean_distance(g)
  }
  tr[i] <- mean(trtemp)
  md[i] <- mean(mdtemp)
}

# Divde by max to scale tr and md similarly
plot(steps, tr/max(tr), lwd=3, type="l", 
     col="blue", xlab=expression(log[10](p)),
     ylab="Transitivity and Mean Distance",
     ylim=c(0,1))
lines(steps, md/max(md), lwd=3, col="red")

# tr2 <- numeric(len)
# md2 <- numeric(len)
# for(i in 1:len) {
#   trtemp2 <- numeric(1000)
#   mdtemp2 <- numeric(1000)
#   for(j in 1:100) {
#     g <- sample_smallworld(1, gorder(g1)*15, 2, 10^steps[i])
#     trtemp2[j] <- transitivity(g)
#     mdtemp2[j] <- mean_distance(g)
#   }
#   tr2[i] <- mean(trtemp2)
#   md2[i] <- mean(mdtemp2)
# }
# 
# plot(steps, tr2/max(tr2), lwd=3, type="l",
#      col="blue", xlab=expression(log[10](p)),
#      ylab="Transitivity and Mean Distance")
# lines(steps, md2/max(md2), lwd=3, col="red")

set.seed(346)
sw1 <- c()
sw1_deg <- list()
for(i in 1:1000) {
  r <- rewire_lattice(gorder(g1), 2, 0.05)
  rt <- transitivity(r, type = "global")
  sw1 <- append(sw1, rt)
  sw1_deg[[i]] <- degree(r)
}
hist(sw1, main = "Distribution of Transitivity (Small World, f1)",
     xlab = "Transitivity Value", col = "lightblue")
summary(sw1) # Min: 0.2981, Q1: 0.4125, Med: 0.4367, Mean: 0.4349, Q3: 0.4586, Max: 0.5159
hist(sw1_deg[[1000]])
hist(degree(g1))

### Required Degree Sequence
set.seed(346)
g1_degs <- degree(g1)
rds1 <- c()
for(i in 1:1000) {
  r <- sample_degseq(g1_degs, method="simple.no.multiple")
  rt <- transitivity(r, type = "global")
  rds1 <- append(rds1, rt)
}
hist(rds1, main = "Distribution of Transitivity (Required Degree Sequence from f1)",
     xlab = "Transitivity Value", col = "lightblue")
summary(rds1) # Min: 0.1292, Q1: 0.2123, Med: 0.2400, Mean: 0.2381, Q3: 0.2585, Max: 0.3415

## Friendship network 2 (3 months after network 1)
transitivity(g2, type = "global") # 0.5609756
mean_distance(g2) # 2.26

### Erdos-Renyi
set.seed(346)
er2 <- c()
for(i in 1:1000) {
  # g2 has 85 edges and 26 nodes; set p = 0.2615384615 for r to have an expected edge count of 85
  # 0.2615384615 * 26(25)/2 = 85
  r <- random_graph(gorder(g2), 0.2615384615)
  rt <- transitivity(r, type = "global")
  er2 <- append(er2, rt)
}
hist(er2, main = "Distribution of Transitivity (Erdos-Renyi, f2)",
     xlab = "Transitivity Value", col = "lightblue")
summary(er2) # Min: 0.1115, Q1: 0.2294, Med: 0.2540, Mean: 0.2548, Q3: 0.2816, Max: 0.3810

### Small World
set.seed(346)
sw2 <- c()
for(i in 1:1000) {
  r <- rewire_lattice(gorder(g2), 3, 0.05)
  rt <- transitivity(r, type = "global")
  sw2 <- append(sw2, rt)
}
hist(sw2, main = "Distribution of Transitivity (Small World, f2)",
     xlab = "Transitivity Value", col = "lightblue")
summary(sw2) # Min: 0.4010, Q1: 0.5076, Med: 0.5330, Mean: 0.5296, Q3: 0.5573, Max: 0.6061

### Required Degree Sequence
set.seed(346)
rds2 <- c()
g2_degs <- degree(g2)
for(i in 1:1000) {
  r <- sample_degseq(g2_degs, method="simple.no.multiple")
  rt <- transitivity(r, type = "global")
  rds2 <- append(rds2, rt)
}
hist(rds2, main = "Distribution of Transitivity (Required Degree Sequence from f2)",
     xlab = "Transitivity Value", col = "lavender")
summary(rds2) # Min: 0.2780, Q1: 0.3355, Med: 0.3498, Mean: 0.3518, Q3: 0.3690, Max: 0.4457

## Friendship network 3 (3 months after network 2)
transitivity(g3, type = "global") # 0.5542312
mean_distance(g3) # 1.94

### Erdos-Renyi
set.seed(346)
er3 <- c()
for(i in 1:1000) {
  # g3 has 97 edges and 26 nodes; set p = 0.2984615385 for r to have an expected edge count of 97
  # 0.2984615385 * 26(25)/2 = 85
  r <- random_graph(gorder(g3), 0.2984615385)
  rt <- transitivity(r, type = "global")
  er3 <- append(er3, rt)
}
hist(er3, main = "Distribution of Transitivity (Erdos-Renyi, f3)",
     xlab = "Transitivity Value", col = "lightblue")
summary(er3) # Min: 0.1756, Q1: 0.2675, Med: 0.2915, Mean: 0.2915, Q3: 0.3166, Max: 0.4095

### Small World
set.seed(346)
sw3 <- c()
for(i in 1:1000) {
  r <- rewire_lattice(gorder(g3), 4, 0.075)
  rt <- transitivity(r, type = "global")
  sw3 <- append(sw3, rt)
}
hist(sw3, main = "Distribution of Transitivity (Small World, f3)",
     xlab = "Transitivity Value", col = "lightblue")
summary(sw3) # Min: 0.4343, Q1: 0.5217, Med: 0.5451, Mean: 0.5440, Q3: 0.5681, Max: 0.6402

### Required Degree Sequence
set.seed(346)
rds3 <- c()
g3_degs <- degree(g3)
for(i in 1:1000) {
  r <- sample_degseq(g3_degs, method="simple.no.multiple")
  rt <- transitivity(r, type = "global")
  rds3 <- append(rds3, rt)
}
# Keep getting an error when trying to use 'vl' method:
# Error in sample_degseq(g3_degs, method = "vl") : 
#   At gengraph_graph_molloy_optimized.cpp:453 : 
#   graph_molloy_opt::make_connected() returned FALSE : vertex 20 has degree 0, Internal error, likely a bug in igraph
#     --> Vertex 20 of the graph g3 has degree 2 though...
hist(rds3, main = "Distribution of Transitivity (Required Degree Sequence from f3)",
     xlab = "Transitivity Value", col = "lavender")
summary(rds3) # Min: 0.3969, Q1: 0.4398, Med: 0.4541, Mean: 0.4552, Q3: 0.4684, Max: 0.5328

## Friendship network 4 (3 months after network 3)
transitivity(g4, type = "global") # 0.4467085
mean_distance(g4) # 1.9

### Erdos-Renyi
set.seed(346)
er4 <- c()
for(i in 1:1000) {
  # g4 has 86 edges and 26 nodes; set p = 0.2646153846 for r to have an expected edge count of 86
  # 0.2646153846 * 26(25)/2 = 86
  r <- random_graph(gorder(g4), 0.2646153846)
  rt <- transitivity(r, type = "global")
  er4 <- append(er4, rt)
}
hist(er4, main = "Distribution of Transitivity (Erdos-Renyi, f4)",
     xlab = "Transitivity Value", col = "lightblue")
summary(er4) # Min: 0.1391, Q1: 0.2321, Med: 0.2564, Mean: 0.2579, Q3: 0.2848, Max: 0.3810

### Small World
set.seed(346)
sw4 <- c()
for(i in 1:1000) {
  r <- rewire_lattice(gorder(g4), 3, 0.12)
  rt <- transitivity(r, type = "global")
  sw4 <- append(sw4, rt)
}
hist(sw4, main = "Distribution of Transitivity (Small World, f4)",
     xlab = "Transitivity Value", col = "lightblue")
summary(sw4) # Min: 0.2889, Q1: 0.4146, Med: 0.4470, Mean: 0.4472, Q3: 0.4776, Max: 0.5754

### Required Degree Sequence
set.seed(346)
rds4 <- c()
g4_degs <- degree(g4)
for(i in 1:1000) {
  r <- sample_degseq(g4_degs, method="simple.no.multiple")
  rt <- transitivity(r, type = "global")
  rds4 <- append(rds4, rt)
}
# Same error using 'vl' method that I kept getting in friendship network 3
hist(rds4, main = "Distribution of Transitivity (Required Degree Sequence from f4)",
     xlab = "Transitivity Value", col = "lavender")
summary(rds4) # Min: 0.2727, Q1: 0.3480, Med: 0.3621, Mean: 0.3627, Q3: 0.3809, Max: 0.4326

# ============================================================================

# Is the change in transitivity between different points in time (from g1 to g2
# to g3 to g4) significant?

# Bootstrapped distributions of transitivity for each friendship network
set.seed(346)
bootdist1 <- c()
bootdist2 <- c()
bootdist3 <- c()
bootdist4 <- c()

v <- vertices(sample(V(g1), gorder(g1), replace=TRUE))

# Slow
for(i in 1:1000) {
  bootdist1 <- append(bootdist1, transitivity(vboot(g1, v)))
  bootdist2 <- append(bootdist2, transitivity(vboot(g2, v)))
  bootdist3 <- append(bootdist3, transitivity(vboot(g3, v)))
  bootdist4 <- append(bootdist4, transitivity(vboot(g4, v)))
}

hist(bootdist1, main = "Bootstrapped distribution of transitivity from f1",
    xlab = "Transitivity", col = "turquoise")
sd(bootdist1) # 0.03789465

hist(bootdist2, main = "Bootstrapped distribution of transitivity from f2",
     xlab = "Transitivity", col = "lightblue")
sd(bootdist2) # 0.02383357

hist(bootdist3, main = "Bootstrapped distribution of transitivity from f3",
     xlab = "Transitivity", col = "lavender")
sd(bootdist3) # 0.01716918

hist(bootdist4, main = "Bootstrapped distribution of transitivity from f4",
     xlab = "Transitivity", col = "gold")
sd(bootdist4) # 0.02482708

## Bootstrap distributions of differences between transitivity in f1 and f2
d_b1_b2 <- bootdist2 - bootdist1
hist(d_b1_b2, main = "Bootstrapped distribution of the difference in transitivity values between friendship networks 1 and 2",
     xlab = "Transitivity at time 2 - Transitivity at time 1", cex.main = 0.9)
quantile(d_b1_b2, c(0.025, 0.975)) # [-0.009775407, 0.163111070] <- contains 0; not significantly different

## Bootstrap distribution of differences between transitivity in f2 and f3
d_b2_b3 <- bootdist3 - bootdist2
hist(d_b2_b3, main = "Bootstrapped distribution of the difference in transitivity values between friendship networks 2 and 3",
     xlab = "Transitivity at time 3 - Transitivity at time 2", cex.main = 0.9)
quantile(d_b2_b3, c(0.025, 0.975)) # [0.01122056, 0.12545244] <- doesn't contain 0; 
                                   # time 3 transitivity is significantly greater than time 2 transitivity

## Bootstrap distribution of differences between transitivity in f3 and f4
d_b3_b4 <- bootdist4 - bootdist3
hist(d_b3_b4, main = "Bootstrapped distribution of the difference in transitivity values between friendship networks 3 and 4",
     xlab = "Transitivity at time 4 - Transitivity at time 3", cex.main = 0.9)
quantile(d_b3_b4, c(0.025, 0.975)) # [-0.19592724, -0.08205825] <- doesn't contain 0; 
                                   # time 4 transitivity is significantly less than time 3 transitivity

# Distributions of transitivity obtained by using the jackknife procedure
set.seed(346)

jackdist1 <- jackknife(g1, "transitivity")
jackdist2 <- jackknife(g2, "transitivity")
jackdist3 <- jackknife(g3, "transitivity")
jackdist4 <- jackknife(g4, "transitivity")

hist(jackdist1, main = "Jackknife distribution of transitivity from f1",
     xlab = "Transitivity", col = "turquoise")
sd(jackdist1) # 0.01565041

hist(jackdist2, main = "Jackknife distribution of transitivity from f2",
     xlab = "Transitivity", col = "lightblue")
sd(jackdist2) # 0.01975708

hist(jackdist3, main = "Jackknife distribution of transitivity from f3",
     xlab = "Transitivity", col = "lavender")
sd(jackdist3) # 0.01259701

hist(jackdist4, main = "Jackknife distribution of transitivity from f4",
     xlab = "Transitivity", col = "lavender")
sd(jackdist4) # 0.01379902

## Jackknife distributions of differences between transitivity in f1 and f2
d_j1_j2 <- jackdist2 - jackdist1
hist(d_j1_j2, main = "Jackknife distribution of the difference in transitivity values between friendship networks 1 and 2",
     xlab = "Transitivity at time 2 - Transitivity at time 1", cex.main = 0.9)
quantile(d_j1_j2, c(0.025, 0.975)) # [0.008527503, 0.084110262] <- doesn't contain 0; 
                                   # time 2 transitivity is significantly greater than time 1 transitivity

## Jackknife distribution of differences between transitivity in f2 and f3
d_j2_j3 <- jackdist3 - jackdist2
hist(d_j2_j3, main = "Jackknife distribution of the difference in transitivity values between friendship networks 2 and 3",
     xlab = "Transitivity at time 3 - Transitivity at time 2", cex.main = 0.9)
quantile(d_j2_j3, c(0.025, 0.975)) # [-0.03612817, 0.03887031 ] <- contains 0; not significant

## Jackknife distribution of differences between transitivity in f3 and f4
d_j3_j4 <- jackdist4 - jackdist3
hist(d_j3_j4, main = "Jackknife distribution of the difference in transitivity values between friendship networks 3 and 4",
     xlab = "Transitivity at time 4 - Transitivity at time 3", cex.main = 0.9)
quantile(d_j3_j4, c(0.025, 0.975)) # [-0.12757702, -0.08184937 ] <- doesn't contain 0; 
                                   # time 4 transitivity is significantly less than time 3 transitivity

# ============================================================================

# Is the change in number of 2-cores between different points in time (from g1 to g2
# to g3 to g4) significant?

# Bootstrapped distributions of number of 2-cores for each friendship network
set.seed(346)
cbootdist1 <- c()
cbootdist2 <- c()
cbootdist3 <- c()
cbootdist4 <- c()

cv <- vertices(sample(V(g1), gorder(g1), replace=TRUE))

# Slow
# for(i in 1:1000) {
#   cbootdist1 <- append(cbootdist1, numcores(vboot(g1, v), 2))
#   cbootdist2 <- append(cbootdist2, numcores(vboot(g2, v), 2))
#   cbootdist3 <- append(cbootdist3, numcores(vboot(g3, v), 2))
#   cbootdist4 <- append(cbootdist4, numcores(vboot(g4, v), 2))
# }
# 
# hist(cbootdist1, main = "Bootstrapped distribution of number of 2-cores from f1",
#      xlab = "Number of 2-cores", col = "turquoise")
# sd(cbootdist1) # 0.03789465
# 
# hist(cbootdist2, main = "Bootstrapped distribution of number of 2-cores transitivity from f2",
#      xlab = "Number of 2-cores", col = "lightblue")
# sd(cbootdist2) # 0.02383357
# 
# hist(cbootdist3, main = "Bootstrapped distribution of number of 2-cores transitivity from f3",
#      xlab = "Number of 2-cores", col = "lavender")
# sd(cbootdist3) # 0.01716918
# 
# hist(cbootdist4, main = "Bootstrapped distribution of number of 2-cores transitivity from f4",
#      xlab = "Number of 2-cores", col = "gold")
# sd(cbootdist4) # 0.02482708
# 
# ## Bootstrap distributions of differences between transitivity in f1 and f2
# d_b1_b2 <- bootdist2 - bootdist1
# hist(d_b1_b2, main = "Bootstrapped distribution of the difference in transitivity values between friendship networks 1 and 2",
#      xlab = "Transitivity at time 2 - Transitivity at time 1", cex.main = 0.9)
# quantile(d_b1_b2, c(0.025, 0.975)) # [-0.009775407, 0.163111070] <- contains 0; not significantly different
# 
# ## Bootstrap distribution of differences between transitivity in f2 and f3
# d_b2_b3 <- bootdist3 - bootdist2
# hist(d_b2_b3, main = "Bootstrapped distribution of the difference in transitivity values between friendship networks 2 and 3",
#      xlab = "Transitivity at time 3 - Transitivity at time 2", cex.main = 0.9)
# quantile(d_b2_b3, c(0.025, 0.975)) # [0.01122056, 0.12545244] <- doesn't contain 0; 
# # time 3 transitivity is significantly greater than time 2 transitivity
# 
# ## Bootstrap distribution of differences between transitivity in f3 and f4
# d_b3_b4 <- bootdist4 - bootdist3
# hist(d_b3_b4, main = "Bootstrapped distribution of the difference in transitivity values between friendship networks 3 and 4",
#      xlab = "Transitivity at time 4 - Transitivity at time 3", cex.main = 0.9)
# quantile(d_b3_b4, c(0.025, 0.975)) # [-0.19592724, -0.08205825] <- doesn't contain 0; 
# # time 4 transitivity is significantly less than time 3 transitivity
# 
# # Distributions of transitivity obtained by using the jackknife procedure
# set.seed(346)
# 
# jackdist1 <- jackknife(g1, "transitivity")
# jackdist2 <- jackknife(g2, "transitivity")
# jackdist3 <- jackknife(g3, "transitivity")
# jackdist4 <- jackknife(g4, "transitivity")
# 
# hist(jackdist1, main = "Jackknife distribution of transitivity from f1",
#      xlab = "Transitivity", col = "turquoise")
# sd(jackdist1) # 0.01565041
# 
# hist(jackdist2, main = "Jackknife distribution of transitivity from f2",
#      xlab = "Transitivity", col = "lightblue")
# sd(jackdist2) # 0.01975708
# 
# hist(jackdist3, main = "Jackknife distribution of transitivity from f3",
#      xlab = "Transitivity", col = "lavender")
# sd(jackdist3) # 0.01259701
# 
# hist(jackdist4, main = "Jackknife distribution of transitivity from f4",
#      xlab = "Transitivity", col = "lavender")
# sd(jackdist4) # 0.01379902
# 
# ## Jackknife distributions of differences between transitivity in f1 and f2
# d_j1_j2 <- jackdist2 - jackdist1
# hist(d_j1_j2, main = "Jackknife distribution of the difference in transitivity values between friendship networks 1 and 2",
#      xlab = "Transitivity at time 2 - Transitivity at time 1", cex.main = 0.9)
# quantile(d_j1_j2, c(0.025, 0.975)) # [0.008527503, 0.084110262] <- doesn't contain 0; 
# # time 2 transitivity is significantly greater than time 1 transitivity
# 
# ## Jackknife distribution of differences between transitivity in f2 and f3
# d_j2_j3 <- jackdist3 - jackdist2
# hist(d_j2_j3, main = "Jackknife distribution of the difference in transitivity values between friendship networks 2 and 3",
#      xlab = "Transitivity at time 3 - Transitivity at time 2", cex.main = 0.9)
# quantile(d_j2_j3, c(0.025, 0.975)) # [-0.03612817, 0.03887031 ] <- contains 0; not significant
# 
# ## Jackknife distribution of differences between transitivity in f3 and f4
# d_j3_j4 <- jackdist4 - jackdist3
# hist(d_j3_j4, main = "Jackknife distribution of the difference in transitivity values between friendship networks 3 and 4",
#      xlab = "Transitivity at time 4 - Transitivity at time 3", cex.main = 0.9)
# quantile(d_j3_j4, c(0.025, 0.975)) # [-0.12757702, -0.08184937 ] <- doesn't contain 0; 
# # time 4 transitivity is significantly less than time 3 transitivity