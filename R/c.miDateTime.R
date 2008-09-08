`c.miDateTime` <-
function (..., recursive = FALSE) 
structure(c(unlist(lapply(list(...), unclass))), class = "miDateTime")

