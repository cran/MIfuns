`Ops.miDateTime` <-
function(e1,e2){
	f1 <- as.miDateTime(as.character(e1))
	f2 <- as.miDateTime(as.character(e2))
	e1[!is.na(f1)] <- f1[!is.na(f1)]
	e2[!is.na(f2)] <- f2[!is.na(f2)]
	NextMethod()
}

