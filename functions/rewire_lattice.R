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
  
  if (k < 0 || k > n - 1) {
    stop("Invalid node neighbor distance.")
  }
  
  if (p < 0 || p > 1) {
    stop("Probability value invalid.")
  }
  
  g <- make_lattice(length = n, dim = 1, nei = k, circular = TRUE)
  
  # Make the lattice
  # for(i in 1:n) {
  #   kn <- ego(g, k, i)
  #   print(kn)
  #   nl <- kn[[1]]
  #   print(nl)
  #   for(j in nl) {
  #     if (!are_adjacent(g, i, j) && i != j) {
  #       g <- add_edges(g, c(i,j))       
  #     }
  #   }
  # }
  
  return(g)
}

tg <- rewire_lattice(15, 3, 0.1)
plot(tg, layout=layout_in_circle(tg))
