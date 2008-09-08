`parameter.plot.table` <-
function(parameter.array){
# create history, density and Gelman-Rubin-Brooks plots
# return value is a table of summary stats for the parameters

	# create history, density and Gelman-Rubin-Brooks plots
	mcmc.history(parameter.array)
	mcmc.density(parameter.array,n=50)
	x1 = mcmc.list(lapply(1:n.chains,function(i) mcmc(parameter.array[,i,]))) # format required by CODA
	try(gelman.plot(x1),TRUE)

	# summary stats on parameters
	psummary = summary(x1)
	ptable = cbind(psummary$statistics,psummary$quantiles)
	neff = try(effectiveSize(x1),TRUE)
	if(exists("neff")){
		ptable = cbind(psummary$statistics,psummary$quantiles,neff)
		dimnames(ptable)[[2]][ncol(ptable)] = "Effective N"
	}
	ptable
}

