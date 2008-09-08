`[[.annotated` <-
function(x,...){
##First, do to y whatever you would normally do to y, based on the next class in the class vector (which happens to be "data.frame").
	y <- NextMethod(x,...)
##Now add the special stuff. Grab the annotations, which is a list.
	a <- attributes(x)$annotations
##Element selection can be called with one, two, or three arguments.  The first argument is the object from which the selection is to be made (x).  The second argument is typically the name of an element, or column in the case of a data frame.  The third argument could be "drop".  If the user tried to select a specific value from a specific row (x[[i,j]]) then we're going to ignore the extras, and just give them that.  We're also not going to handle x[[i,drop]], because we either haven't taken the time to understand it or have forgotten what it does.  We will, however, handle invocations like x[[1]] and x[["CL"]], which result in two arguments.  Exactly one of these arguments will be found in the dots list.  In that case, we have already selected the column of interest, but now we must select the attribute of interest as well.
	if(length(list(...))==1) attr(y,"annotations") <- lapply(a,"[[",...)
	y
}

