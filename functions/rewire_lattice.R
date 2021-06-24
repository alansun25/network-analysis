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
    stop("Probability value must be between 0 and 1.")
  }
  
  g <- make_lattice(length = n, dim = 1, nei = k, circular = TRUE)
  size <- gsize(g)
  
  # Note: We delete edge 1 each time, which means what was edge 2 before is now edge 1. The for-loop
  # ensures we do this until all initial edges have been rewired.
  for(i in 1:size) {
    edges <- E(g)
    
    # Randomly choose one of the nodes incident on the initial edge to be the starting node
    start <- ends(g, edges[1])[sample(1:2, 1)]

    prob <- runif(1)
    if (prob < p) {
      rewire <- sample(1:n, 1) # Choose random node to rewire to
      
      # No self-loops or multiple edges
      while (rewire == start || are_adjacent(g, rewire, start)) {
        rewire <- sample(1:n, 1)
      }
      
      g <- delete_edges(g, edges[1]) # Remove initial edge
      g <- add_edges(g, c(start, rewire)) # Rewire the edge
    }
  }
  
  return(g)
}

rewired <- rewire_lattice(10, 2, 0.345)
gsize(rewired)
plot(rewired, layout=layout_in_circle(rewired))
