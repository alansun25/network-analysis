library(igraph)

# WIP: Find the number of k-cores in a graph

numcores <- function(g, k) {
  if (!is_igraph(g)) {
    stop("Object must be an igraph graph.")
  }
  
  if (k < 0) {
    stop("Vertices cannot have negative degree.")
  }
  
  if (k != round(k)) {
    stop("k value must be an integer.")
  }
  
  cores <- coreness(g)
  
}

r <- erdos.renyi.game(50, 0.02, "gnp")
numcores(r, 2)
