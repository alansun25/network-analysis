library(igraph)

# Vertex Bootstrapping

vboot <- function(g, v) {
  if (!is_igraph(g)) {
    stop("Object must be an igraph graph.")
  }

  gboot <- make_empty_graph(0, FALSE)
  n <- vcount(g)
  dens <- edge_density(g)
  
  gboot <- gboot + v
  
  for(i in 1:n) {
    for(j in 1:n) {
      v1 <- V(gboot)[i]
      v2 <- V(gboot)[j]
      
      # No self-loops or multiple edges
      if (i != j && !are_adjacent(gboot, v1, v2)) {
        prob <- runif(1)
        
        # The probability of there being an edge between any two vertices is the edge density.
        #
        # If the two artificial vertices correspond to the same real vertex and the random number
        # prob is less than the edge density of g, we add an edge between the two artifical vertices.
        # - OR -
        # If the two articial vertices correspond to different real vertices, add an edge betwen them
        # if that same edge exists in g.
        if ((v1$name == v2$name && prob < dens) || are_adjacent(g, v1$name, v2$name)) {
          gboot <- add_edges(gboot, c(v1, v2))
        }
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
  
  return(gboot)
}

# Tests
t <- make_lattice(length=10, dim=1, nei=2, circular = TRUE)
# length(vertex_attr(t))
# vt <- vboot(t, vertices(sample(V(t), gorder(t), replace=TRUE)))
# 
# length(E(t))
# length(E(vt))
# 
# plot(t)
# plot(vt, layout=layout_nicely(vt))
# 
# asdf <- make_ring(10) %>% 
#   set_vertex_attr("label", value = LETTERS[1:10]) %>%
#   set_vertex_attr("color", index=1, value="blue")
# vasdf <- vboot(asdf, vertices(sample(V(asdf), gorder(asdf), replace=TRUE)))
# vertex_attr(vasdf)
# vertex_attr(asdf)

