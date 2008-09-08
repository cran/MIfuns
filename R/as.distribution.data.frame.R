`as.distribution.data.frame` <-
function(x,z,influence,...){
	y <- as.annotated(x,z=z,influence=influence,...)
	class(y) <- c("distribution",class(y))
	y
}

