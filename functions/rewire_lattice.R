library(igraph)

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
  
  if (k < 0) {
    stop("Neighbors cannot have negative distance.")
  }
  
  if (p < 0 || p > 1) {
    stop("Probability value must be between 0 and 1.")
  }
  
  l <- make_lattice(length = n, dim = 1, nei = k, circular = TRUE)
  e <- make_empty_graph(n, directed = FALSE)

  edges_visited <- make_empty_graph(n, directed=FALSE)
  
  for(i in 1:n) {
    n_edges <- incident(l, i)
    for(j in 1:length(n_edges)) {
      prob <- runif(1)
      
      v1 <- ends(l, n_edges[j])[1]
      v2 <- ends(l, n_edges[j])[2]
      
      # Only rewire if probability p is exceeded and if edge j has not already been visited.
      if (!are_adjacent(edges_visited, v1, v2)) {
        if (prob < p) {
          rewire <- sample(1:n, 1) # Choose random node to rewire to
          
          # No self-loops
          while (rewire == i) {
            rewire <- sample(1:n, 1)
          }
          
          e <- add_edges(e, c(i, rewire))
        } else {
          if (!are_adjacent(e, v1, v2)) {
            e <- add_edges(e, c(v1, v2)) 
          }
        }
        
        edges_visited <- add_edges(edges_visited, c(v1, v2)) 
      }
    }
  }
  
  # No multiple-edges (may cause the number of total edges in the graph to decrease)
  e <- simplify(e)

  return(e)
}

rewired <- rewire_lattice(8, 2, 0.5)
gsize(rewired)
plot(rewired, layout=layout_in_circle(rewired))
