library(igraph)

# Random Graph Generation
#
# Generates a random, undirected graph.
# Parameters: 
#   n (number of nodes)
#   p (probability of a link between any two nodes; between 0 and 1)
#
# Self-loops are not allowed.
random_graph <- function(n, p) {
  if (n < 0) {
    stop("Graph cannot have a negative number of nodes.")
  }
  
  if (p < 0 || p > 1) {
    stop("Probability value invalid.")
  }
  
  g <- make_empty_graph(n, directed=FALSE)
  
  for(i in 1:n) {
    for (j in i:n) {
      rand <- runif(1)
      if (rand < p && !are_adjacent(g, i, j) && i != j) {
        g <- add_edges(g, c(i,j))
      }
    }
  }
  return(g)
}


# Tests
igraph.options(vertex.size=10)

plot(random_graph(13, 0.13))
plot(random_graph(20, 0.08))
plot(random_graph(5, 0)) # empty graph
plot(random_graph(5, 1)) # complete graph

rg <- random_graph(50, 0.035)
summary(rg)
V(rg)
E(rg)
plot(rg)
plot(rg, layout=layout_in_circle(rg))
plot(rg, layout=layout_as_tree(rg))
plot(rg, layout=layout_nicely(rg))
plot(rg, layout=layout_on_grid(rg))
plot(rg, layout=layout_randomly(rg))
plot(rg, layout=layout_with_fr(rg))
plot(rg, layout=layout_with_kk(rg))

### Degree distribution ###

n <- 1000
p <- 0.01
ddg <- random_graph(n, p) # Expected number of edges: p*n(n-1)/2 = 4995
summary(ddg) # Observed number of edges: 5042
k <- degree(ddg)
p_k <- choose(n-1, k)*(p^k)*(1-p)^(n-1-k)

# Theoretical peak: k = <k> = p(n-1) = 9.99
plot(p_k~k, main='Degree distribution', xlab='Degree', 
     ylab='Probability a random node has degree k',
     pch=16, type='n')
lines(k[order(k)], p_k[order(k)], xlim=range(k), ylim=range(p_k), 
      lty=1, lwd=2)
k[which.max(p_k)] # Observed peak: k = 10

