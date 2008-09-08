`rinvchisq` <-
function(n,df,omega) {
	return(df*omega/rchisq(n, df))
}

