# igraph practice

install.packages("igraph")
library(igraph)

# data sets and code blocks for the textbook
install.packages("sand")
library(sand)

## Chapter 2

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

plot(g.complete)s
plot(g.ring)
plot(g.tree)
plot(g.star)
