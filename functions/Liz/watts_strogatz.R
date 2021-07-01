

w_strogatz <- function(n, k, p) {
  if (n < 0) {
    stop("Graph cannot have a negative number of nodes.")
  }
  
  if (k >= n/2) {
    stop("Invalid node neighbor distance; this is a complete graph.")
  }
  
  if (p < 0 || p > 1) {
    stop("Probability value must be between 0 and 1.")
  }
  
  g_new <- make_empty_graph(n, directed = "FALSE")
  
  for(i in 1:n) {
    for(j in (i + 1) : (i + k)){
     
       j <- (j - 1) %% n + 1 #reindex j 
      prob <- runif(1) #rewire?
      head <- runif(1) #rewire head or tail?

      if (prob < p || are_adjacent(g_new, i, j)) { #rewire given rewire probability OR if the non-rewired edge already exists due to prior rewiring
        if(head > .5) { #i is head
          
          tag <- neighbors(g_new,i) #capturing edges that already exist
          
          if (length (tag) == 0){
            rewire <- sample(V(g_new)[-c(i,j)], 1) #catch no neighbors, choose a random node to attach 
          } else {
          rewire <- sample(V(g_new)[-tag], 1) #sample from non neighbors
          }
          
          g_new <- g_new + edge(i, rewire)
          
        } else { #j is head
          
          tag <- neighbors(g_new,j)
          
          if (length (tag) == 0){
            rewire <- sample(V(g_new)[-c(i,j)], 1)
          } else {
          rewire <- sample(V(g_new)[-tag], 1) #sample from non neighbors
          }
          
          g_new <- g_new + edge(j, rewire)
        }
      
        } else {
        
        g_new <- g_new + edge(c(i,j))
        
      }
    }
  }
  
  return(g_new)
}  
  
g_new <- w_strogatz(n = 10, k =4, p =.9)
plot(g_new, layout = layout_in_circle(g_new))


