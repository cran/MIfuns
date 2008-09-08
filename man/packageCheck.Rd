\name{packageCheck}
\alias{packageCheck}
\title{Load a package and run package-level examples for testing purposes.}
\description{
  packageCheck attempts to load the specified package and to run example() with
  the package name as an argument.
}
\usage{
packageCheck(x,lib.loc=NULL)
}
\arguments{
  \item{x}{atomic character, the name of a single package}
  \item{lib.loc}{the library to check, passed to library()}
}
\details{
 Many packages do not have package-level examples; the call to example() in 
 such cases does nothing (warnings are suppressed).
}
\value{
  an atomic character string: zero if the package does not load or if example() 
  generates an error; otherwise, the package version.
}
\references{http://metruminstitute.org}
\author{Tim Bergsma}
\seealso{
\code{\link{accept}},
\code{\link{acceptance}}
\code{\link{library}}}
\examples{
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
}
\keyword{ manip }

