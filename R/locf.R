`locf` <-
function(x){
	good <- !is.na(x)
	positions <- seq(length(x))
	good.positions <- good * positions
	last.good.position <- cummax(good.positions)
	last.good.position[last.good.position==0] <- NA
	x[last.good.position]
}

