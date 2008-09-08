`packageCheck` <-
function(x,lib.loc=NULL){
	if(!is.character(x))stop("x must be character")
	if(length(x)!=1)stop("x must be atomic")
	success <- library(x,character.only=TRUE,lib.loc=lib.loc,logical.return=TRUE)
	if(!success){
		return(0)
	}
	testResult <- try(suppressWarnings(example(x,local=TRUE)))
	if(inherits(testResult,"try-error")){
		return(0)
	}
	return(packageDescription(x,lib.loc=lib.loc,fields="Version"))
}

