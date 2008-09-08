`as.miDate.numeric` <-
function(x,...){
class(x) <- "Date"
as.miDate(x)
}

