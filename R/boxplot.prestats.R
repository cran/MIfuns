`boxplot.prestats` <-
function(x,coef,...){
	#substitute for boxplot.stats
	#panel.bwplot does not pass extra args to boxplot.stats
	#if substitute for boxplot.stats needs extra args, they can stow away as 
	#attributes of coef, which does get passed.
	#boxplot.stats must also subset wide args, because bwplot only subsets x
##Recall that it was probably panel.bwplot that called this function (even though we actually specified the argument in a higher level function).  Recall also that we vaccinated our x values against having their names stripped by panel.bwplot, by making them immutable.  This is now handy, since x has names.  In fact, since panel.bwplot is going to call 'stats' once for each subset of x, the particular subset of x with which we are now dealing has elements, all of which have the same name!  Let's grab that name now.
	this <- unique(names(x))
##Now we define extra args as a list containing all those attributes of coef except the first (which is the 'real' stats function).  Now, panel.bwplot bothers to subset our data for us, but it does not bother to subset our extras.  So where subsetting makes sense, we'll subset our extra args ourselves.  It makes sense where the arg in question has elements with names, and those we want are the elements with the same names as the unique name worn by all the data points in the data vector:  Very Important Point.  (Note that the next four uses of x are local to the inline function, and have nothing to do with the x passed to boxplot.prestats.)
	extras <- lapply(
		attributes(coef)[-1],
		FUN=function(x,this){
			if(this %in% names(x))return(x[[this]])
			return(x)
		},
		this=this
	)
##When we defined extras above, we ignored the first extra on coef.  We pick it up now as the function to which all the other extras were intended to be passed.
	FUN <- match.fun(attributes(coef)$stats)
##We need do.call soon, which takes the name of a function, and a bunch of args in a list.  So roll all our args into one nice list, not forgetting to include the (now deflated) value of coef itself (we strip its attributes by calling as.numeric).
	args <- c(list(x=x,coef=as.numeric(coef)),extras)
##Who can say what other stuff was supposed to get passed?  If the stats function accepts dots, include them in the args list. 
	if("..." %in% names(as.list(args(FUN)))){
		args <- c(args,list(...))
	}else{
##Conversely, if the function in question does not accept dots, anything we're passing that is not explicitly accepted will cause an error, and must be stripped.
		args <- args[names(args) %in% names(as.list(args(FUN)))]
	}
##Now we have a function name, and a list of args we're sure will be accepted.
	do.call(FUN,args)
}

