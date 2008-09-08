`mcmc.history` <-
function(sims.array){

	n1 = dim(sims.array)[1]
	n2 = dim(sims.array)[2]
	n3 = dim(sims.array)[3]
	x = data.frame(value=as.vector(sims.array),
		chain=rep(rep(1:n2,ea=n1),n3),
		parameter=rep(dimnames(sims.array)[[3]],ea=n1*n2))
	print(xyplot(value~rep(1:n1,n2*n3)|parameter,x,groups=chain,
		panel=panel.superpose,type="l",col=c(1,2,3,4),
		layout=c(1,6),scales=list(cex=1,y=list(relation="free")),
		xlab=list(label="sample",cex=1.2),ylab=list(label="value",cex=1.2),
		par.strip.text=list(cex=1),strip = function(...) strip.default(..., style = 1)))
	NULL
}

