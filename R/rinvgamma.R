`rinvgamma` <-
function(n,df,sigma) {
# shape=alpha=DF
# rate = beta
# mode(rinvgamma)=beta/(alpha+1)
# mean(rinvgamma)=beta/(alpha-1)
# var(rinvgamma)=beta^2/(alpha-1)^2/(alpha-2)

	shape <- df
	rate <- sigma*df
	return(1/rgamma(n, shape, rate))
}

