\name{accept}
\alias{accept}
\title{Document acceptance of an R installation.}
\description{
  Execution of accept() is a procedural act pertininent to installation qualification
  The function logs an act of acceptance of the current installation, noting the 
  conditions of acceptance.  Currently, the conditions include the login of the 
  acceptor, the time of acceptance (GMT) and any contingent packages that 
  survive packageCheck().
}
\usage{
accept(
	contingencies = c(
		"akima",
		"boot",
		"coda",
		"foreign",
		"fork",
		"lattice",
		"locfit",
		"MASS",
		"nlme",
		"R2WinBUGS",
		"reshape",
		"SASxport",
		"survival"
	),
	installMissing=TRUE
)
}
\arguments{
  \item{contingencies}{a character vector of packages that must survive packageCheck
  in order for acceptance to succeed.  Can be NULL.  Defaults are arbitrarily chosen
  to suit the needs of Metrum Research Group, LLC.}
  \item{installMissing}{atomic logical: should install.packages() be attempted for 
  missing contingencies?}
}
\details{
  The file "accept.xml" in .Library is created if it does not exist, and appended
  if it does.  In addition to the acceptor's login and the time, names and 
  versions of any contingent packages are stored.
  
  If installMissing is TRUE, an attempt will be made to install packages not in
  the default library (.Library), before loading is attempted.
}
\value{
  an invisible named vector of version identifiers, where the names are package
  names.
}
\references{http://metruminstitute.org}
\author{Tim Bergsma}
\seealso{
\code{\link{packageCheck}},
\code{\link{acceptance}}}
\examples{
## The function is currently defined as
function(
	contingencies=c(
		"akima",
		"BRugs",
		"boot",
		"coda",
		"foreign",
		"fork",
		"lattice",
		"locfit",
		"MASS",
		"MIfuns",
		"nlme",
		"R2WinBUGS",
		"reshape",
		"SASxport",
		"survival"
	),
	installMissing=TRUE
){
	library(XML)
	check <- NULL
	if(!is.null(contingencies)){
		if(!inherits(contingencies,"character"))
                stop("contingencies must be a character vector of package names, or NULL")
		if(installMissing){
			installed <- installed.packages(lib.loc=.Library)[,1]
			missing <- setdiff(contingencies,installed)
			try(
				install.packages(missing,lib=.Library)
			)
		}
		check <- sapply(contingencies,packageCheck,lib.loc=.Library)
		if(any(check==0))
                stop(paste("check failed for",paste(names(check)[check==0],collapse=", ")))
	}
	acceptor <- Sys.info()["login"]
	time <- Sys.time()
	filepath <- file.path(.Library,"accept.xml")
	as.XMLNode <- function(x,...)UseMethod("as.XMLNode")
	as.XMLNode.XMLDocument <- function(x,...)xmlTreeParse(x,...)$doc$children[[1]]
	as.XMLNode.character <- function(x,...)xmlTreeParse(x,...)$doc$children[[1]]
	if(file.exists(filepath))doc <- as.XMLNode.XMLDocument(filepath,asText=FALSE)
	else doc <- as.XMLNode("<acceptance/>")
	accepted <- newXMLNode("accepted")
	accepted <- addChildren(accepted,newXMLNode("acceptor",acceptor))
	accepted <- addChildren(accepted,newXMLNode("time",as.character(time)))
	for (package in names(check)){
		test <- newXMLNode("test")
		test <- addChildren(test,newXMLNode("package",package))
		test <- addChildren(test,newXMLNode("version",check[package]))
		accepted <- addChildren(accepted,test)
	}
	doc <- addChildren(doc,as.XMLNode(saveXML(accepted),asText=TRUE))
	saveXML(doc,filepath)
	invisible(check)
}
}
\keyword{ manip }

