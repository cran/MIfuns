`covplot.stats` <-
function(x,y,z,influence,coef,do.out=TRUE,transform="x*1",file=NULL,...){
#Apparently, some z are showing up as character.  Coerce to numeric....
z <- as.numeric(z)
##Convert text to expression...
	influence <- parse(text=influence)
	transform <- parse(text=transform)
##Let's define a list to use as the evaluation environment.
env <- list(...)
##If y has a name, let's assign that in the evaluation environment, so it will be available to influence expressions.
	if(!is.null(names(y)))env[[names(y)]] <- y
##The stats function should return the number of data points involved.
	n <- sum(!is.na(x))
##Let's create a spot for values defining the box.
	stats <- c(NA,NA,NA,NA,NA)
##If z is of length one, we trigger the categorical behavior.
	if(length(z) == 1){
##For x in the evaluation environment, consider all values from the appropriate column of distribution object (annotated data frame).  
		env$x <- x
##Of course, assign z.
		env$z <- z
##Now evaluate the influence expression, which likely has x,y, and z.
		vals <- eval(influence,envir=env)
##Define the box thus:  the whiskers are the .025 and .975 quantiles, the center is the median, and the box ends are also the median (zero width box, which will also be invisible).
		stats <- quantile(vals,probs=c(0.025,0.50,0.50,0.50,0.975),na.rm=TRUE)
##Technically we should define the outliers.
		out <- numeric(0)
		if(do.out)out <- vals[vals > max(stats) | vals < min(stats)]
	}else{
##This is what we do for continuous covariates.  For the box, we set x to the median of all passed x values (e.g. bootstrapped theta estimates); we set z to median and 95 percentiles of the z vector (e.g. observed covariate data).  Then we evaluate the influence expression at the one value of x and the three values of z, sorting the result and assigning into the stats vector.
		env$x <- quantile(x,0.5,na.rm=TRUE)
		env$z <- quantile(z,0.025,na.rm=TRUE)
		stats[2] <- median(eval(influence,envir=env)) #median to reduce in the presence of non-atomic influence elements other than x, y, z.
		env$z <- quantile(z,0.5,na.rm=TRUE)
		stats[3] <- median(eval(influence,envir=env))
		env$z <- quantile(z,0.975,na.rm=TRUE)
		stats[4] <- median(eval(influence,envir=env))
		stats[2:4] <- sort(stats[2:4])
##Next we need to define the whiskers.  Because of the funny things that happen when certain values go negative, it's hard to know where the low and high values will originate.  We'll restore x to all passed values (e.g. bootstrap estimates of theta).  Then z can take turns being an extreme low and extreme high value of the passed z values (e.g. observations of a covariate). In each case, we'll evaluate the influence expression at all values of x and the extreme value of z, then clip the result 
## by selecting extreme quantiles.  
		env$x <- x
		env$z <- quantile(z,0.025,na.rm=TRUE)
		z.lo <- quantile(eval(influence,envir=env),c(0.025,0.975),na.rm=TRUE)
		env$z <- quantile(z,0.975,na.rm=TRUE)
		z.hi <- quantile(eval(influence,envir=env),c(0.025,0.975),na.rm=TRUE)
##Two extremes of z, times two extremes of all x evaluated on z, gives four whisker candidates, of uncertain rank.  Assign the range across all four to the whisker slots in stat. 
		stats[c(1,5)] <- range(c(z.lo,z.hi))
##Check to make sure the stats are coherently ordered, and assign out.
		if(any(stats!=sort(stats)))stop("stats not sorted as expected in covplot.stats")
		out <- numeric(0)		
	}
##conf is the notch values.  We don't implement notches (neither does bwplot) so we'll just set them equal to the box ends.
	conf <- stats[c(2,4)]
##Before we go, we'll transform all values as prescribed by the transform expression.  Transform authors have only 'x' at their disposal, which corresponds to values on the x axis.  Both panel and prepanel will call this function, so the plotting and the axes construction will all be coordinated. A stats function returns a list with the appropriate named elements.
	res <- list(
		stats=eval(transform,envir=list(x=stats)),
		n=n,
		stats=eval(transform,envir=list(x=conf)),
		stats=eval(transform,envir=list(x=out))
	)
##If file is specified, we should append to file.
	if(is.null(names(y)[[1]])) names(y)[[1]] <- names(x)[[1]]
	if(!is.null(file)) write(
		paste(
			names(y)[[1]],
			names(x)[[1]],
			median(x,na.rm=TRUE),
			median(z,na.rm=TRUE),
			stats[[1]],
			stats[[2]],
			stats[[3]],
			stats[[4]],
			stats[[5]],
			#Sys.time(),
			sep="\t"
		),
		file,
		append=TRUE
	)
	res	
}

