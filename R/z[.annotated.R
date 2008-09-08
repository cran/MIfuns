`[.annotated` <-
function (x, ...) {
##We start by storing the request that got us here.  cl is a list.
	cl <- match.call()
##The first element of the list is the name of the function that was called: in this case, "[.annotated".  We're going to replace that with the corresponding method for data frame, and then evaluate the call.  In other words, capture what normally would have been done.
	cl[[1]] <- as.name("[.data.frame")

	y <- eval(cl)
##Now we'll deal with the extras.
	a <- attributes(x)$annotations
	dots <- list(...)
##We don't want to do anything in particular with drop, so if present we'll blow it away.
	if("drop" %in% names(dots))dots$drop <- NULL
##Now we have a dropless list, so we can interpret its length unambiguously.  The user is either selecting columns or rows and columns.  In the first case, the columns argument will be first.  But in the second case, the rows argument will be first.  We need to find the columns argument, so that we can grab the annotations that correspond to it.
	if (length(dots)==1) attr(y,"annotations") <- lapply(a,"[",dots[[1]])
	if (length(dots)==2) attr(y,"annotations") <- lapply(a,"[",dots[[2]])
	y
}

