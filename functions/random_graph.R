library(igraph)

# Random Graph Generation
#
# Parameters: 
#   n (number of nodes)
#   p (probability of a link between any two nodes; between 0 and 1)
#
# Note: Self-loops are allowed.
random_graph <- function(n, p) {
  g <- make_empty_graph(n, directed=FALSE)
  for(i in 1:n) {
    for (j in 1:n) {
      rand <- runif(1)
      if (rand <= p && !are.connected(g, i, j)) {
        g <- add_edges(g, c(i,j))
      }
    }
  }
  return(g)
}


# Tests
plot(random_graph(20, 0.35))

