`bin` <-
function(
	x,
	population=x,
	breaks=quantile(
		population,
		probs=c(0,0.25,0.5,0.75,1),
		...
	),
	...
)table(
	cut(
		x,
		breaks=breaks,
		include.lowest=TRUE,
		...
	),
	...
)

