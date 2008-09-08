`rep.miTime` <-
function(x,...){
	y <- NextMethod()
	class(y) <- class(x)
	y
}

