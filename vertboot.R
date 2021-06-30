library(Rcpp)

cppFunction('IntegerMatrix vertboot_matrix_rcpp(IntegerMatrix m1,IntegerVector blist){
  int a;
  int b;
  int num=m1.nrow();
  IntegerMatrix x1(num,num);
  for(int k=0;k<num;k++){
    for(int q=0;q<num;q++){
      x1(k,q)=0;
    }
  }
  for(int i=0;i<num;i++){
    for(int j=(i+1);j<num;j++){
      a=blist[i];
      b=blist[j];
      if(a!=b){
        x1(i,j)=m1(a,b);
      }
      else{
        a=round(R::runif(-0.49,num-0.5));
        b=round(R::runif(-0.49,num-0.5));
        while(a==b){
          b=round(R::runif(-0.49,num-0.5));
        }
        x1(i,j)=m1(a,b);
      }
    }
  }
  return (x1);
}')

g <- erdos.renyi.game(n= 10, p =.5)
plot(g)
blist <- sample(0:(vcount(g) - 1), replace = TRUE)
g_mat <- as.matrix(get.adjacency(g))
sim <- vertboot_matrix_rcpp(g_mat, blist)
g_sim <- graph_from_adjacency_matrix(sim, mode = c("undirected"))
plot(g_sim)
