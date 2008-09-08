`Ops.miDate` <-
function(e1,e2){
	f1 <- as.miDate(as.character(e1),format="%m/%d/%Y")
	f2 <- as.miDate(as.character(e2),format="%m/%d/%Y")
	e1[!is.na(f1)] <- f1[!is.na(f1)]
	e2[!is.na(f2)] <- f2[!is.na(f2)]
	NextMethod()
}

