library(igraph)

# Vertex Bootstrapping

vboot <- function(g) {
  if (!is_igraph(g)) {
    stop("Object must be an igraph graph.")
  }

  gboot <- make_empty_graph(0, FALSE)
  n <- vcount(g)
  
  gboot <- gboot + vertices(sample(V(g), n, replace=TRUE))
  
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
  
  # Transfer vertex attributes from g to gboot
  if (length(vertex_attr(g)) != 0) {
    v_attr <- vertex_attr_names(g)
    for(i in 1:length(V(gboot))) {
      v <- V(gboot)[i]
      for(j in 1:length(v_attr)) {
        v_attr_name <- v_attr[j]
        v_attr_value <- vertex_attr(g, v_attr_name, v$name)
        gboot <- set_vertex_attr(gboot, v_attr_name, v, v_attr_value)
      }
    }
  }
  
  # Transfer edge attributes from g to gboot
  if (length(edge_attr(g)) != 0) {
    e_attr <- edge_attr_names(g)
    for(i in 1:length(E(gboot))) {
      e <- E(gboot)[i]
      for(j in 1:length(e_attr)) {
        e_attr_name <- e_attr[j]
        e_attr_value <- edge_attr(g, e_attr_name)[match(e, E(g))]
        gboot <- set_edge_attr(gboot, e_attr_name, e, e_attr_value)
      }
    }
  }
  
  return(gboot)
}

# Tests
t <- make_lattice(length=10, dim=1, nei=2, circular = TRUE)
length(vertex_attr(t))
vt <- vboot(t)

length(E(t))
length(E(vt))

plot(t)
plot(vt, layout=layout_nicely(vt))

asdf <- make_ring(10) %>% 
  set_vertex_attr("label", value = LETTERS[1:10]) %>%
  set_vertex_attr("color", index=1, value="blue")
vasdf <- vboot(asdf)
vertex_attr(vasdf)
vertex_attr(asdf)

qwer <- make_ring(10) %>%
  set_edge_attr("label", value = LETTERS[1:10])
vqwer <- vboot(qwer)
plot(qwer)
plot(vqwer)
E(vqwer)
edge_attr(qwer)
edge_attr(vqwer)



