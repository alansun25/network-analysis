library(igraph)

# Random Graph Generation
#
# Generates a random, undirected graph
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
      # If the random number is less than the probability of a link
      # between nodes i and j and i and j are not alreadt connected,
      # create an edge between i and j.
      if (rand <= p && !are.connected(g, i, j)) {
        g <- add_edges(g, c(i,j))
      }
    }
  }
  return(g)
}


# Tests
igraph.options(vertex.size=10)

plot(random_graph(10, 0.1))
plot(random_graph(13, 0.13))
plot(random_graph(50, 0.035))

rg <- random_graph(15, 0.08)
summary(rg)
V(rg)
E(rg)
plot(rg, layout=layout_in_circle(rg))
plot(rg, layout=layout_as_tree(rg))
plot(rg, layout=layout_nicely(rg))
plot(rg, layout=layout_on_grid(rg))
plot(rg, layout=layout_randomly(rg))
plot(rg, layout=layout_with_fr(rg))
plot(rg, layout=layout_with_kk(rg))
