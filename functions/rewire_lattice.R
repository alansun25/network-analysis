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
  l <- make_lattice(length = n, dim = 1, nei = k, circular = TRUE)
  
  if (k == n/2) {
    return(l)
  }
  
  if (n < 0) {
    stop("Graph cannot have a negative number of nodes.")
  }
  
  if (k < 0) {
    stop("Neighbors cannot have negative distance.")
  }
  
  if (k > n/2) {
    stop("Neighborhood is not well defined.")
  }
  
  if (p < 0 || p > 1) {
    stop("Probability value must be between 0 and 1.")
  }

  e <- make_empty_graph(n, directed = FALSE)

  edges_visited <- make_empty_graph(n, directed=FALSE)
  
  for(i in 1:n) {
    n_edges <- incident(l, i)
    for(j in 1:length(n_edges)) {
      prob <- runif(1)
      
      v1 <- ends(l, n_edges[j])[1]
      v2 <- ends(l, n_edges[j])[2]
      
      start <- c(v1, v2)[sample(1:2, 1)]
      
      # Only rewire if probability p is exceeded and if edge j has not already been visited.
      if (!are_adjacent(edges_visited, v1, v2)) {
        if (prob < p) {
          rewire <- sample(1:n, 1) # Choose random node to rewire to
          
          # No self-loops
          while (rewire == start || are_adjacent(e, rewire, start)) {
            print(edge_density(e))
            rewire <- sample(1:n, 1)
          }
          
          e <- add_edges(e, c(start, rewire))
        } else {
          if (!are_adjacent(e, v1, v2)) {
            e <- add_edges(e, c(v1, v2)) 
          }
        }
        
        edges_visited <- add_edges(edges_visited, c(v1, v2)) 
      }
    }
  }

  return(e)
}

rewired <- rewire_lattice(10, 4, 1)
gsize(rewired)
plot(rewired, layout=layout_in_circle(rewired))
