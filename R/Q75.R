`Q75` <-
function(x) {
  Q75 <- as.double(quantile(x,probs=c(0.75)))
  return(Q75)
}

