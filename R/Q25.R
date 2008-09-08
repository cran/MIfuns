`Q25` <-
function(x) {
  Q25 <- as.double(quantile(x,probs=c(0.25)))
  return(Q25)
}

