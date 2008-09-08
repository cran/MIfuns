`mcmc.density` <-
function(sims.array,n=50){

# n = number of points at which the density is calculated

	n1 = dim(sims.array)[1]
	n2 = dim(sims.array)[2]
	n3 = dim(sims.array)[3]
	x = data.frame(value=as.vector(sims.array),
		chain=rep(rep(1:n2,ea=n1),n3),
		parameter=rep(dimnames(sims.array)[[3]],ea=n1*n2))
	x = data.frame(parameter=rep(unique(x$parameter),ea=n),
		value=as.vector(sapply(unique(x$parameter),function(x,n,sim.list){
		density(sim.list$value[sim.list$parameter==x],
		n=n,na.rm=TRUE)$x},n=n,sim.list=x)),
		frequency=as.vector(sapply(unique(x$parameter),function(x,n,sim.list){density(
		sim.list$value[sim.list$parameter==x],
		n=n,na.rm=TRUE)$y},n=n,sim.list=x)))

	print(xyplot(frequency~value|parameter,x,scales="free",type="l",col=1,
		layout=c(0,min(16,length(unique(x$parameter)))),
		par.strip.text=list(cex=1),strip = function(...) strip.default(..., style = 1)))
	NULL
}

