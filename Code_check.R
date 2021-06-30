g <- graph.formula(1-2, 2-3, 2-4, 3-4)
plot(g)

gboot <- make_empty_graph(0, FALSE)
n <- vcount(g)

samp <- c(1,2,3,4)
gboot <- gboot + vertices(samp)

# If the edge exists in g, add it to gboot
for(i in 1:n) {
  for(j in 1:n) {
    v1 <- V(gboot)[i]
    v2 <- V(gboot)[j]
    
    if (i!= j # No self-loops
        && !are_adjacent(gboot, v1, v2) # No multiple edges
        && are_adjacent(g, v1$name, v2$name)) {
      gboot <- add_edges(gboot, c(v1, v2))
    }
  }
}
