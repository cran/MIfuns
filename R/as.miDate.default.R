`as.miDate.default` <-
function(x,...){
Sys.setenv(TZ = "GMT")
x <- as.Date(x,...)
class(x) <- c("miDate",class(x))
return(x)
}

