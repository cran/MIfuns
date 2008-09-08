`riwish` <-
function(s,df,Prec){
   if (df<=0) stop ("Inverse Wishart algorithm requires df>0")
   R <- diag(sqrt(2*rgamma(s,(df + s  - 1:s)/2)))
   R[outer(1:s, 1:s,  "<")] <- rnorm (s*(s-1)/2)
   S <- t(solve(R))%*% chol(Prec)
   return(t(S)%*%S)
}

