\name{hash}
\alias{hash}
\title{Supply comment characters with output}
\description{
  Any expression that generates output can be wrapped in a call to hash().  The usual
  output will be returned, with a hash character at the start of each line.
}
\usage{
hash(x)
}
\arguments{
  \item{x}{an R expression}
}
\details{
  This is useful for embedding the result of an expression in a script.
}
\value{
used for side effects
}
\references{http://metruminstitute.org}
\author{Tim Bergsma}
\section{Warning}{
  Don't "hash" an expression that directly or indirectly calls a function being
  debugged.  The debug output will be 'sunk' to a connection.  If you get in 
  this situation, recover by typing \code{sink(NULL)} until output is restored.
}
\seealso{
\code{\link{file}},
\code{\link{sink}}}
\examples{
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
}
\keyword{manip}
