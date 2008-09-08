`prepanel.covplot` <-
function(x,...){
##Unpack the args into the local environment.
	args <- list(...)
	for(nm in names(args))assign(nm,args[[nm]])
##split the entire data vector into sets, based on the names of individual values.  It is a Very Good Thing that these values have names, and that the names were not stripped!
	sets <- split(x,names(x))
##split returns a list, no now we can list-apply the stats function, which will be the same one used by the panel function.
	meta.stats <- lapply(sets,stats,coef)
##metastats is a list, each element of which is a list returned by stats, each of which lists themselves contain an element named 'stats', which is the only thing we want.  So we'll grab the stats element from each element in metastats, then unlist that thing to get all possible values in one vector, and let xlim be the range of that vector.  The next use of the word 'stats' is just a local variable name.
	stats <- lapply(meta.stats,"[[","stats")
	list(xlim=range(unlist(stats)))
}

