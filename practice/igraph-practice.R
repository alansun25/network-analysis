# igraph practice

install.packages("igraph")
library(igraph)

# data sets and code blocks for the textbook
install.packages("sand")
library(sand)
install.packages('igraphdata')
library(igraphdata)

## Chapter 2 - Manipulating Network Data

### undirected
g <- graph_from_literal(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6, 4-7, 5-6, 6-7)
plot(g)
print_all(g)
neighbors(g, 5)
degree(g)
is_connected(g)
diameter(g)

g2 <- graph_from_literal(1-2, 2-3, 3-4, 4-2, 5-6, 6-7, 7-8, 8-6, 9-10, 10-11, 11-9)
plot(g2)
clusters(g2)

### directed
dg <- graph_from_literal(John-+Emily, Emily++Mark, Julia-+John, Julia-+Mark)
plot(dg)
print_all(dg)
is_simple(dg)
neighbors(dg, 'Julia')
degree(dg, mode='in')
degree(dg, mode='out')
is_connected(dg, mode='weak')
is_connected(dg, mode='strong')

### induced subgraph
h <- induced_subgraph(g, 1:5)
plot(h)
print_all(h)

### vertex attributes
V(dg)$name # names of the people represented by nodes
(V(dg)$gender <- c('M', 'F', 'M', 'F')) # equipping dg's nodes with a gender attribute

### edge attributes (weights)
wg <- g
E(wg)$weight <- runif(ecount(wg)) # generate random edge weights
is_weighted(wg)

### special graphs
g.complete <- make_full_graph(25)
g.ring <- make_ring(25)
g.tree <- make_tree(25, mode='undirected')
g.star <- make_star(25, mode='in')

plot(g.complete)
plot(g.ring)
plot(g.tree)
plot(g.star)

## Chapter 3 - Visualizing Network Data

l <- make_lattice(c(5,5,5))
data(aidsblog)

igraph_options(vertex.size=3, vertex.label=NA, edge.arrow.size=0.5)

plot(l, layout=layout_in_circle)

plot(aidsblog, 
     layout=layout_in_circle(aidsblog, order=order(degree(aidsblog, mode='out'))),)
title('AIDS Blog Network')

### Fruchterman and Reingold (spring-embedder)
plot(l, layout=layout_with_fr)
plot(aidsblog, layout=layout_with_fr)

### Kamada and Kawai (energy-placement)
plot(l, layout=layout_with_kk)
plot(aidsblog, layout=layout_with_kk)

### Decorating graphs
data(lazega)
set.seed(345)
laz.colors <- c('red', 'green', 'blue')[V(lazega)$Office] # Office location - Vertex color
laz.shape <- c('circle', 'square')[V(lazega)$Practice] # Lawyer practice - Vertex shape
laz.size <- 3.5*sqrt(V(lazega)$Years) # Years with the company - Vertex size
laz.label <- V(lazega)$Seniorty # Seniority - Vertex label
igraph.options(vertex.label=laz.label, vertex.label.cex=0.5)
plot(lazega, layout=layout_with_kk, 
     vertex.color=laz.colors, 
     vertex.shape=laz.shape, 
     vertex.size=laz.size, 
     vertex.label=laz.label)

## Chapter 4

data(karate)

### Degree distribution
hist(degree(karate), col='lavender', 
     main='Degree distribution in Zachary\'s karate club network', 
     xlab='Vertex degree', xlim=c(0,20))

### Weighted degree (strength) distribution
### the sum of the weights of edges incident to a given vertex
hist(strength(karate), col='skyblue',
     main='Strength distribution in Zachary\'s karate lcub network',
     xlab='Vertex strength')

### Average neighbor degree vs vertex degree
data(yeast)
plot(degree(yeast), knn(yeast, V(yeast))$knn,
     log='xy', col='turquoise', pch=16,
     xlab='Log vertex degree', ylab='Log average neighbor degree')

### network cohesion
cliques(karate)
max_cliques(karate)

dyad_census(simplify(aidsblog))
triad_census(simplify(aidsblog))

edge_density(karate)
edge_density(induced_subgraph(karate, vids=neighborhood(karate, 1, 1)[[1]]))
edge_density(induced_subgraph(karate, vids=neighborhood(karate, 1, 34)[[1]]))

# clustering
transitivity(karate)
transitivity(yeast)
transitivity(aidsblog)

reciprocity(aidsblog, mode='default') # directed edges approach
reciprocity(aidsblog, mode='ratio') # dyads approach

giant <- decompose(yeast)[[1]]
vertex_connectivity(giant)
articulation_points(giant)
