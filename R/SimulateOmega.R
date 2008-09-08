`SimulateOmega` <-
function(nsim,df,Cov){
	nomega <- dim(Cov)[1]
	ncols <- nomega*(nomega+1)/2
	res <- matrix(nrow=nsim, ncol=ncols)
	for( isim in 1:nsim) {
	   res[isim,] <- myriwish(nomega,df,Cov)
    }
	return(res)
}

