`acceptance` <-
function(){
	library(XML)
	filepath <- file.path(.Library,"accept.xml")
	as.XMLNode <- function(x,...)UseMethod("as.XMLNode")
	as.XMLNode.XMLDocument <- function(x,...)xmlTreeParse(x,...)$doc$children[[1]]
	as.XMLNode.character <- function(x,...)xmlTreeParse(x,...)$doc$children[[1]]
	if(file.exists(filepath))return(as.XMLNode.XMLDocument(filepath,asText=FALSE))
	return(NULL)
}

