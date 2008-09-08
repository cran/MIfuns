`hash` <-
function(x){
	con <- file()
	sink(con)
	result <- try(x)
	if(!inherits(result,"try-error"))print(result)
	comments <- paste("#",readLines(con))
	sink(NULL)
	close(con)
	writeLines(comments)	
}

