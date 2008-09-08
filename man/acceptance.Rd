\name{acceptance}
\alias{acceptance}
\title{List the history of acceptance.}
\description{
 This function reads "accept.xml" in .Library, if it exists, and prints it
 with the default method.
}
\usage{
acceptance()
}
\details{
  Data is displayed as xml markup.
}
\value{
 an object of class XMLNode
  ...
}
\references{http://metruminstitute.org}
\author{Tim Bergsma}
\seealso{
\code{\link{accept}},
\code{\link{packageCheck}}}
\examples{
## The function is currently defined as
function(){
	library(XML)
	filepath <- file.path(.Library,"accept.xml")
	as.XMLNode <- function(x,...)UseMethod("as.XMLNode")
	as.XMLNode.XMLDocument <- function(x,...)xmlTreeParse(x,...)$doc$children[[1]]
	as.XMLNode.character <- function(x,...)xmlTreeParse(x,...)$doc$children[[1]]
	if(file.exists(filepath))return(as.XMLNode.XMLDocument(filepath,asText=FALSE))
	return(NULL)
  }
}
\keyword{ manip}

