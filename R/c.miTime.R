`c.miTime` <-
function (..., recursive = FALSE) 
structure(c(unlist(lapply(list(...), unclass))), class = "miTime")

