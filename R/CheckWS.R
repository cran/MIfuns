`CheckWS` <-
function(x) {
   res <- FALSE
   for (i in 1:nchar(x)) if(substring(x,i,i) == " ") res <- TRUE
   return(res)
}

