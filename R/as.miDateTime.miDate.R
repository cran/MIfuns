`as.miDateTime.miDate` <-
function(x,y,...){
if (!inherits(y,"miTime"))stop("second argument must be of class miTime")
if (length(x)!=length(y))stop("lengths of arguments differ")
x <- as.numeric(x) + as.numeric(y)
class(x) <- c("miDateTime")
return(x)
}

