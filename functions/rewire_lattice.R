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
  if ((n %% 2 == 0 && k >= n/2) || (n %% 2 == 1 && k >= n/2 - 1)) {
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
  # l <- layout_in_circle(e)
  # plot(e, layout=l)
  # Sys.sleep(2.5)
  
  for(i in 1:n) {
    for(j in (i + 1):(i + k)) {
      j <- (j - 1) %% n + 1 # Wrap around if j > n.
      
      prob <- runif(1)
      
      if (prob < p || are_adjacent(e, i, j)) {
        # If either node has degree n - 1, make the other one the starting node by default.
        # Otherwise, randomly select one to start as the head.
        if (degree(e, i) == n - 1) {
          start <- j
        } else if (degree(e, j) == n - 1) {
          start <- i
        } else {
          start <- c(i, j)[sample(1:2, 1)] 
        }
        
        if (degree(e, i) == n - 1 && degree(e, j) == n - 1) {
          # If both nodes have degree n - 1, a loop or multiple edge is inevitable.
          rewire <- sample(V(e), 1)
        } else {
          adj_start <- neighbors(e, start) # Get all nodes that already form edges with start.
          if (length(adj_start) > 0) {
            rewire <- sample(V(e)[-adj_start], 1)
            while (rewire == start) {
              rewire <- sample(V(e)[-adj_start], 1)
            }
          } else {
            rewire <- sample(V(e)[-c(i,j)], 1)
          }
        }
        
        e <- add_edges(e, c(start, rewire))
      } else {
        e <- add_edges(e, c(i, j))
      }
      
      # plot(e, layout=l)
      # Sys.sleep(2.5)
    }
  }
  
  return(e)
}

rewired <- rewire_lattice(20, 6, 0.5)
gsize(rewired)
plot(rewired, layout=layout_in_circle(rewired))
