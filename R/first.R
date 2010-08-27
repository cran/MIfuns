#first returns  the first element in a vector, 'within' each level, where condition is TRUE,
#repeated for each element sharing the level
first <- function(x,where=rep(TRUE,length(x)),within=rep(1,length(x)),...){
	if(missing(x))if(missing(where))stop('one of x or where must be specified')
	if(missing(x))x <- seq(length.out=length(where))
	where <- as.logical(where)
	if(length(where)!=length(x))warning('where is repeated to match length x')
	where <- rep(where,length.out=length(x))
	if (!is.list(within))within <- list(within)
	check <- sapply(within,length)
	if(any(check!=length(x)))stop('All indicies must have same length as x.')
	na <- sapply(within,function(x)any(is.na(x)))
	if(any(na))warning('within has NA')
	unidex <- as.numeric(factor(do.call(paste,within)))
	x[match(paste(unidex,rep(TRUE,length(x))),paste(unidex,where))]
}
#nth returns the nth element in x where where is TRUE, 'within' each level,
#repeated for each element sharing the level. Negative values count from the end of the vector.
nth <- function(x,where=rep(TRUE,length(x)),within=rep(1,length(x)),n=1,...){
	n=as.integer(n)
	if(length(n)!=1)stop('n must have length one')
	if(missing(x))if(missing(where))stop('one of x or where must be specified')
	if(missing(x))x <- seq(length.out=length(where))
	where <- as.logical(where)
	if(length(where)!=length(x))warning('where is repeated to match length x')
	where <- rep(where,length.out=length(x))
	if(!is.list(within))within <- list(within)
	if(n==0)return(rep(NA,length(x)))
	if(n<0)return(rev(nth(x=rev(x),where=rev(where),n=-n,within=lapply(within,rev))))
	if(n==1)return(first(x=x,where=where,within=within))
	where[unique(first(where=where,within=within))] <- FALSE
	nth(x=x,where=where,n=n-1,within=within)
}
last <- function(x,where=rep(TRUE,length(x)),within=rep(1,length(x)),n=-1,...)nth(x=x,where=where,within=within,n=-1,...)

#logicals
distance <- function(where,within=rep(1,length(where)),n=1,...)1:length(where)-nth(where=where,within=within,n=n,...)
before   <- function(where,within=rep(1,length(where)),n=1,...)distance(where=where,within=within,n=n,...)<0
at       <- function(where,within=rep(1,length(where)),n=1,...)distance(where=where,within=within,n=n,...)==0
after    <- function(where,within=rep(1,length(where)),n=1,...)distance(where=where,within=within,n=n,...)>0

