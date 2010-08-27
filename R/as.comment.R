`as.comment` <- function(x,...)UseMethod("as.comment")

`as.comment.default` <- function(x,...){
	x <- as.logical(x)
	class(x) <- c("comment",class(x))
	x
}

`as.comment.comment` <- function(x,...)x

`as.character.comment` <- function(x,...)format(x)

`c.comment` <- function(...,recursive=FALSE)structure(c(unlist(lapply(list(...), unclass))), class="comment")

`as.data.frame.comment` <- function (x, row.names = NULL, optional = FALSE, ...) 
{
    nrows <- length(x)
    nm <- paste(deparse(substitute(x), width.cutoff = 500), collapse = " ")
    if (is.null(row.names)) {
        if (nrows == 0) 
            row.names <- character(0)
        else if (length(row.names <- names(x)) == nrows && !any(duplicated(row.names))) {
        }
        else row.names <- .set_row_names(nrows)
    }
    names(x) <- NULL
    value <- list(x)
    if (!optional) 
        names(value) <- nm
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    value
}

`[.comment` <- function (x, ..., drop = TRUE) 
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[")
    class(val) <- cl
    val
}

`[[.comment` <- function (x, ..., drop = TRUE) 
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[[")
    class(val) <- cl
    val
}

`rep.comment` <- function(x,...){
	y <- NextMethod()
	class(y) <- class(x)
	y
}

`format.comment` <- function(x,...)unclass(ifelse(x,"C","."))

`print.comment` <- function(x,...){
	print(format(x),...,quote=FALSE)
	invisible(x)
}
xtfrm.comment <- function(x)as.numeric(x)


`hide.data.frame` <- function(x,where,why,...){
	if(!"C" %in% names(x))x$C <- as.comment(FALSE)
	where <- as.logical(where)
	x$C[where] <- TRUE
	if(!why %in% names(x)){
		x[[why]] <- as.flag(0)
	}else{
		x[[why]] <- as.flag(x[[why]])
	}
	x[[why]][where] <- as.flag(1)
	x[,c("C",setdiff(names(x),"C"))]
}

`hide` <- function(x,...)UseMethod("hide")

