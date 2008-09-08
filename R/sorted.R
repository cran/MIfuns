`sorted` <-
function(x,on)identical(
	x,
	x[
		with(
			x,
			do.call(
				order,
				lapply(
					on,
					as.name
				)
			)
		),
	]
)

