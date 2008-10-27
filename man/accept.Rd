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
\keyword{ manip }

