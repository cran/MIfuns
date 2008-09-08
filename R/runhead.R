`runhead` <-
function(x){#not like last observation
	n <- x!=prev(x)
	n[[1]] <- TRUE
	n
}

