library(igraph)
library(rlang)

# Rewiring a Lattice
#
# Creates and rewires a lattice given the following parameters:
#   n (number of nodes in the initial lattice)
#   k (distance of farthest neighbor each node will be connected to in the initial lattice)
#   p (rewiring probability)
#
# p = 0 corresponds to the regular, initial lattice (high clustering, lacks small world property)
# p = 1 corresponds to a random network (low clustering, displays small world property)
#
# The point of interest here is to rewire the lattice to have low average path length (small world)
# as well as high clustering.
rewire_lattice <- function(n, k, p) {
  if (n < 0) {
    stop("Graph cannot have a negative number of nodes.")
  }
  
  if (k < 0 || k > n - 1) {
    stop("Invalid node neighbor distance.")
  }
  
  if (p < 0 || p > 1) {
    stop("Probability value invalid.")
  }
  
  g <- make_lattice(length = n, dim = 1, nei = k, circular = TRUE)
  size <- gsize(g)
  
  # I think I may to do need this, but not too sure... was running into problems of using an edge set of a
  # different graph
  # Duplicate initial lattice so we only change the duplicate and can still access the original edges
  # tg <- duplicate(g)
  
  for(i in 1:size) {
    edges <- E(g)
    
    # Randomly choose one of the nodes incident on edge i to be the starting node
    start <- ends(g, edges[i])[sample(1:2, 1)]

    prob <- runif(1)
    if (prob < p) {
      g <- delete_edges(g, edges[i]) # Remove initial edge
      rewire <- sample(1:n, 1) # Choose random node to rewire to
      
      # No self-loops or multiple edges
      while (rewire == start || are_adjacent(g, rewire, start)) {
        rewire <- sample(1:n, 1)
      }
      
      g <- add_edges(g, c(start, rewire)) # Rewire the edge
    }
  }
  
  return(g)
}

rewired <- rewire_lattice(20, 2, 0.4)
gsize(rewired)
plot(rewired, layout=layout_in_circle(rewired))

# tt <- make_lattice(length = 3, dim = 1, nei = 2, circular = TRUE)
# E(tt)
# tt <- delete_edges(tt, E(tt)[1])
# E(tt)
