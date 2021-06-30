library(igraph)

# Vertex Jackknife

jackknife <- function(g, stat) {
  stat <- tolower(stat)
  
  if (!is_igraph(g)) {
    stop("Object must be an igraph graph.")
  }
  
  if (stat != "density" && stat != "transitivity") {
    stop("Provided statistic not currently supported.")
  }
  
  n <- vcount(g)
  jk_stats <- c()
  
  for(i in 1:n) {
    jg <- delete_vertices(g, V(g)[i])
    if (stat == "density") {
      jk_stats <- append(jk_stats, edge_density(jg))
    } else {
      jk_stats <- append(jk_stats, transitivity(jg, type="global"))
    }
  }
  
  return(jk_stats)
}

s <- make_star(30, "undirected")
plot(s)
(jackknife(s, "density"))

t <- make_lattice(length=12, dim=1, nei = 2)
plot(t)
(jackknife(t, "density"))
(jackknife(t, "transitivity"))
