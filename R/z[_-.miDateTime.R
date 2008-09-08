`[<-.miDateTime` <-
function(x,...,value){
if (!as.logical(length(value))) 
        return(x)
    value <- as.miDateTime(value)
    cl <- oldClass(x)
    class(x) <- class(value) <- NULL
    x <- NextMethod(.Generic)
    class(x) <- cl
    x
}

