`as.csv.filename.character` <-
function(x,...){
class(x) <- c("csv.filename",class(x))
return(x)
}

