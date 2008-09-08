`panel.covplot` <-
function(
	x,
	ref,
	rlim=NULL,
	rcol="grey90",
	pcol="green",
	...
){
##rlim is the limits for the shaded reference region, in x coordinates. rcol can be passed from the higher level functions; default is a very light grey.  pcol is the color for the special case where the box represents a term on the right side that is identical to the single term on the left side (i.e. distribution of variable of interest).  ref corresponds to the vertical line representing a point estimate for the variable of interest. x, of course, is the data to be plotted.

##Immediately we make x immutable, so that names won't be stripped later by panel.bwplot, and thus will be available when bwplot calls stats.
	class(x) <- "immutable"
##Now we make the box a solid rather than dashed line.
	#fix line type
	box.umbrella <- original.umbrella <- trellis.par.get("box.umbrella")
	box.umbrella$lty <- 1
##Figure out what the key variable is, and turn it green where present.
	#special color for distribution of key parameter, if present
	ychar <- names(attr(list(...)$coef,"y"))
	box.umbrella$col <- rev(
		ifelse(
			unique(names(x))==ychar,
			pcol,
			box.umbrella$col
		)
	)
	trellis.par.set("box.umbrella", box.umbrella)
##We don't really plot boxes for categorical covariates.  Since we don't want to mess with the underlying plot code, we'll just make the boxes transparent.  We know something is categorical (or think we know) because there is only one value at which the influence on the parameter is evaluated.
	#transparent boxes for categorical covariates
	zLen <- sapply(attr(list(...)$coef,"z"),length)[unique(names(x))]
	box.rectangle <- original.rectangle <- trellis.par.get("box.rectangle")
	box.rectangle$col <- rev(
		ifelse(
			zLen==1,
			"transparent",
			box.rectangle$col
		)
	)
	trellis.par.set("box.rectangle",box.rectangle)
##We need to clean up rlim, which is null as default but is better behaved if a value is present.  If no one specified a reference region, we'll create a zero-width region that hides behind ref, which must be present.
	#shade relevance region
	if(is.null(rlim))rlim <- rep(ref,2)
##If rlim is passed in as a single value, we'll interpret it as a fractional interval on ref.
	if(length(rlim)==1)rlim <- ref * (1 + c(-rlim,+rlim))
##If rlim is passed as two values, we'll treat as absolute.
	if(length(rlim)!=2)stop("rlim must two absolute values, or a fraction of ref")
##Now we'll do some plotting.  Lay down the reference rectangle.
	panel.rect(
		xleft=rlim[1],
		ybottom=0,
		xright=rlim[2],
		ytop=1+length(unique(names(x))),
		border="transparent",
		col=rcol
	)
##Then we'll plot the data.  bwplot.formula, the function that called this one, should have already called a prepanel function. panel.bwplot will call whatever we specified for stats, or boxplot.stats by default.  Last, put in the vertical reference line and restore the par settings out of courtesy to other users.
	#plot data
	panel.bwplot(x,...)
	#add ref line
	panel.abline(v=ref)
	#cleanup
	box.rectangle <- original.rectangle
	trellis.par.set("box.rectangle",box.rectangle)
	box.umbrella <- original.umbrella
	trellis.par.set("box.umbrella", box.umbrella)
}

