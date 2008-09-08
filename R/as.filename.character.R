`as.filename.character` <-
function(x,...){
class(x) <- c("filename",class(x))
return(x)
}

