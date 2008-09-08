metaSub <- function(x,...)UseMethod("metaSub")

metaSub.filename <- 
function (x, names, ...){
       if(!length(x) %in% c(1,length(names)))stop("x must be atomic, or as long as 'names'")
       if(length(x)==1) x <- rep(x,length(names))
       names(names) <- x
       ms <- function(name,names)metaSub(
           paste(
	       readLines(
	           names(names[names==name])
	       ),
	       collapse="\n"
	   ),
	   names=name,
	   ...
       )               
       invisible(sapply(names,ms,names))                                                                                                                                                   
 }
 
 