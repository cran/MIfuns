`%nested.in%` <- function (a,b) b %nests% a
`%nests%` <- function (a,b)!any(crosses(a,b))
`crosses` <- function(a,b)duplicated(
	cbind(
		as.data.frame(a),
		as.data.frame(b)
	)
)!=
duplicated(
	as.data.frame(b)
)



