`as.annotated.data.frame` <-
function(x,...){
##Grab the extra arguments.
	extras <- list(...)
##Find out how long they are.
	extents <- sapply(extras,length)
##Make sure each extra has one value for each column in the data frame.  That's what it means to be 'annotated'.
	if(any(extents != length(x)))stop("arguments must be of same length as x")
##Since extras correspond to data frame columns, name them thus, based on order.  Note that you're asking for trouble if the names in x are not unique (allowed in R).
	extras <- lapply(extras,"names<-",names(x))
##Store the extras in an attribute named 'annotations'.
	attr(x,"annotations") <- extras
##And assign a new class to x, return.
	class(x) <- c("annotated",class(x))
	x
}

