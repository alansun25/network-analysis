library(igraph)

# Rewires a lattice given the following parameters:
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
  # Can't rewire a complete graph because every node is linked to every other node already.
  if (k >= n/2) {
    return(make_lattice(length = n, dim = 1, nei = k, circular = TRUE))
  }
  
  if (k < 0) {
    stop("Neighbors cannot have negative distance.")
  }
  
  if (n < 0) {
    stop("Graph cannot have a negative number of nodes.")
  }
  
  if (p < 0 || p > 1) {
    stop("Probability value must be between 0 and 1.")
  }
  
  e <- make_empty_graph(n, FALSE)
  
  # Chose to do it like this because it is more space efficient than iterating by edge 
  # and having two extra graphs
  for(i in 1:n) {
    for(j in (i + 1):(i + k)) {
      j <- (j - 1) %% n + 1 # Wrap around if j > n
      
      prob <- runif(1)
      
      if (prob < p) {
        start <- c(i, j)[sample(1:2, 1)] # Randomly select node to start as head
        adj_start <- neighbors(e, start) # Get all nodes that already form edges with start
        
        if (length(adj_start) > 0) {
          rewire <- sample(V(e)[-adj_start], 1)
          while (rewire == start) {
            rewire <- sample(V(e)[-adj_start], 1)
          }
        } else {
          rewire <- sample(V(e)[-c(i,j)], 1)
          while (rewire == start) {
            rewire <- sample(V(e)[-c(i,j)], 1)
          }
        }
        
        e <- add_edges(e, c(start, rewire))
      } else {
        # WIP: This doesn't let every edge get added into graph e, but if I remove the if statement it 
        # allows for multiple edges.
        if (!are_adjacent(e, i, j)) {
          e <- add_edges(e, c(i, j))
        }
      }
    }
  }
  
  return(e)
}

rewired <- rewire_lattice(10, 4, 1)
gsize(rewired)
plot(rewired, layout=layout_in_circle(rewired))
