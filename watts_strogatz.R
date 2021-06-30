

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
  
  g_new <- make_empty_graph(n, directed = "FALSE")
  
  for(i in 1:n) {
    for(j in (i + 1) : (i + k)){
      j <- (j - 1) %% n + 1
      prob <- runif(1)
      print(i)
      print(j)
      if (prob < p) {
        rewire <- sample(1:n, 1)
        #check that the edge is new.  Need to update this. 
        g_new <- g_new + edge(c(i, rewire)) #add edges between the node
      } else {
        g_new <- g_new + edge(c(i,j))
      }
    }
  }
  
  return(g_new)
}  
  
plot(g_new, layout = layout_in_circle(g_new))
