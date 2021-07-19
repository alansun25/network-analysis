library(igraph)

# Vertex Jackknife

jackknife <- function(g, stat) {
  stat <- tolower(stat)
  
  if (!is_igraph(g)) {
    stop("Object must be an igraph graph.")
  }
  
  if (stat != "density" && stat != "transitivity" && stat != "meandegree" && stat != "corenessnum") {
    stop("Provided statistic not currently supported.")
  }
  
  n <- vcount(g)
  jk_stats <- c()
  
  for(i in 1:n) {
    jg <- delete_vertices(g, V(g)[i])
    if (stat == "density") {
      jk_stats <- append(jk_stats, edge_density(jg))
    } else if (stat == "transitivity") {
      jk_stats <- append(jk_stats, transitivity(jg, type="global"))
    } else if (stat == "meandegree") {
      jk_stats <- append(jk_stats, mean(degree(jg)))
    } else {
      jg_coreness <- coreness(jg)
      jk_stats <- append(jk_stats, sum(jg_coreness > 1))
    }
  }
  
  return(jk_stats)
}

# Tests
# s <- make_star(30, "undirected")
# plot(s)
# (jackknife(s, "density"))
# 
# t <- make_lattice(length=12, dim=1, nei = 2)
# plot(t)
# (jackknife(t, "density"))
# (jackknife(t, "transitivity"))
# (jackknife(t, "meandegree"))
# (jackknife(t, "corenessnum"))
