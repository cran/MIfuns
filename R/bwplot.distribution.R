`bwplot.distribution` <-
function(
	x,
	data, 
	coef=1.5,
	stats=covplot.stats,
	panel=panel.covplot,
	prepanel=prepanel.covplot,
	transform="x*1",
	pch=1,
	xlab=NULL,
	ref=NULL,
	...
){
##Make sure x is a formula.
	if(!inherits(x,"formula"))stop("x must be a formula")
##The first element of the formula, when converted to a list, is going to be the tilde.  The second element will be the left hand side of the formula, assuming there is only one response term.  This is the only case we can handle with bwplot.distribution.  (Sorry).
	ychar <- as.character(x[[2]])
##Default xlab is the response term.
	if(is.null(xlab))xlab <- ychar
##Expressions in influence may want to refer to the value of the response term.  We capture it here, as the median of the values in the y column.  Then we name it, so that the name itself can be evaluated.
	y <- median(data[[ychar]],na.rm=TRUE)
	names(y) <- ychar
##We're going to set a default value for ref, so every plot will have one of those vertical lines.  We're going to set up a function call so that we can control exactly what is present.  We use do.call to execute this call.  What we want to be present is y, which we pass as a list (do.call needs a list).  Whatever the name of y is, it will be known as 'x' inside of the function, since that is the only argument name specified in the (inline) function definition.  We'll treat y as x, and then evaluate the transform expression, which is x*1 by default (no transform).
	if(is.null(ref)) ref <- do.call(function(x)eval(parse(text=transform)),list(y))
##x is a formula, and someone put a function in package 'stats' that extracts terms from a forumla.  We'll use this to recover the names of the terms, which is much easier than parsing the forumula itself.
	terms <- attr(terms(x),"term.labels")
##data frames are essentially lists of columns, so we can 'list-apply' a function to make every column numeric.  I think this dumps attributes as well.
	clean <- as.data.frame(lapply(data,as.numeric))
##To use bwplot the way we want, we need to pass all the data specified by terms as a single vector.  Otherwise, bwplot will see y~x+z and plot the sum of x and z for each unique value of y.  What we'll do instead is stack x and z into a single vector to plot.
	long <- stack(clean,select=terms)
##stack() has created a data frame with values and indicies. $ind is a factor, with levels in no particular order, as far as I can tell.  We want to recreate this factor, but order the levels in the order in which they occur.  That corresponds to the order in which they are stacked.  Actually, we'll reverse that order, to cause bwplot to display these levels from the top down instead of the bottom up.  The order of the actual data is not affected.
	long$ind <- factor(as.character(long$ind),levels=rev(unique(long$ind)))
##Next comes the most conceptually challenging piece of the whole enterprise.  We have a very fundamental problem:  we need the stats function to calculate some very special stats for us.  bwplot promises to call whatever stats function we happen to specify, and it promises to pass coef to that function for us, and one or two other things besides (do.out?).  Unfortunately, it does not pass 'dots'.  (And it does not pass go, and does not collect $200.)  That means we cannot directly provide extra arguments for our special stats function to consider when considering the data that bwplot has subsetted and passed to 'stats'.  The current solution is to pass extra arguments (needed by covplot.stats) as hitch-hikers on coef.  Since we are sure coef is going to stats, other args can ride along as attributes on coef.  The next few lines load the hitch-hikers.
	attributes(coef) <- c(
		list(
			stats=stats,
			y=y,
			transform=transform
		),
		attributes(data)$annotations	
	)
##An important technical issue was ignored in the help given above.  When 'stats' receives 'coef' from bwplot, it does precious little good that there are a lot of fancy parasites clinging to it.  It will still only look like one argument with one natural value.  Somebody needs to unload those hitch-hikers, or perhaps aliens, and provide them with independent identities (forged passports?).  That somebody is going to be boxplot.prestats:  instead of calling the stats argument specified, we instruct bwplot to call boxplot.prestats, who is an expert in getting aliens off the boat, so to speak.  This means that 'stats' ITSELF, whatever it happens to be (usually covplot.stats) travels incognito, as an attribute of coef.  boxplot.prestats will intercept coef, unpack the aliens, and send them off to whatever is specified by stats.

##We define values and ind in the local environment, which will be searched by default if we don't pass 'data' to bwplot.  We also name the values with their corresponding indicies, so each value carries with it a memory of its origin.
	values <- long$values
	names(values) <- long$ind
	ind <- long$ind
##Now we create the trellis object by calling bwplot.  It will get displayed when returned.  We could have displayed it directly, but this approach lets us tinker if we must.  When bwplot sees ind~values, it will subset 'values' by unique entries in 'ind', and create boxes (and whiskers) for each level of 'ind'  Notice that the passed value for 'stats' is summarily ignored; it's hidden in coef; boxplot.prestats is hardcoded instead.  This is really transparent to the user: boxplot.prestats will call the user-specified 'stats' with the user specified args, after the 'border crossing' is complete.  bwplot will pass coef to boxplot.prestats, and not care at all what happens after that.  Note that the actual function dispatched is bwplot.formula, because ind~values is a formula.
	object <- bwplot(
		ind~values,
		coef=coef,
		panel=panel,
		prepanel=prepanel,
		stats = boxplot.prestats,
		pch=pch,
		xlab=xlab,
		ref=ref,
		...
	)
	object	
}

