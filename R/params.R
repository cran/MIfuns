params <-
function(within,by='name',type='parameter',...){
	library(XML)
	if(length(by)!=1)stop('by must have length one')
	if(is.na(by))return(NA)
	tree <- xmlParse(within,asText=TRUE)
	apath <- paste('//',type,'/@',by,sep='')
	result <- xpathSApply(tree,apath)
	free(tree)
	if(!length(result))return(character(0))
	return(result)
}

