`[[.miDateTime` <-
function (x, ..., drop = TRUE) 
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[[")
    class(val) <- cl
    val
}

