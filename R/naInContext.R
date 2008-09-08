naInContext <- function(x,context,search){
	guilty <- !complete.cases(x[,search])
	keys <- interaction(x[,context])
	x[keys %in% keys[guilty],]
}
