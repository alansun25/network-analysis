# igraph practice

install.packages("igraph")
library(igraph)

## toy graphs

### undirected
g <- graph_from_literal(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6, 4-7, 5-6, 6-7)
plot(g)
print_all(g)

### directed
dg <- graph_from_literal(John-+Emily, Emily++Mark, Julia-+John, Mark++Julia)
plot(dg)
print_all(dg)
