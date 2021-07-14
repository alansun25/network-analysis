steps<- seq(-4, -.05, .1)
len <- length(steps)
cl <- numeric(len)
apl <- numeric(len)
ntrials <- 10
for (i in 1:len) {
  cltemp <- numeric(ntrials)
  apltemp <- numeric(ntrials)
  for (j in 1:ntrials){
    g <- watts.strogatz.game(1, 25, 10, 10^steps[i]) #.001 to .794, density is .02
    cltemp[j] <-transitivity(g)
    apltemp[j] <- average.path.length(g)
  }
  cl[i] <- mean(cltemp)
  apl[i] <- mean(apltemp)
}

plot(steps, cl/max(cl), ylim = c(0,1), lwd = 3, type = 'l', col = "blue")
lines(steps, apl/max(apl), lwd = 3, col = "red")




















steps <- seq(-4, -0.05, 0.1)
len <- length(steps)
tr <- numeric(len)
md <- numeric(len)
ntrials <- 10
for(i in 1:len) {
  trtemp <- numeric(ntrials)
  mdtemp <- numeric(ntrials)
  for(j in 1:ntrials) {
    g <- rewire_lattice(gorder(g1)*10, 2, 10^steps[i]) #density is .16, vcount is 26
    trtemp[j] <- transitivity(g)
    mdtemp[j] <- mean_distance(g)
  }
  tr[i] <- mean(trtemp)
  md[i] <- mean(mdtemp)
}

# Divde by max to scale tr and md similarly
plot(steps, tr/max(tr), lwd=3, type="l", 
     col="blue", xlab="friendship",
     ylab="Transitivity and Mean Distance")
lines(steps, md/max(md), lwd=3, col="red")

graph.density(g1) #.19
