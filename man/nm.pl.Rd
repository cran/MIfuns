\name{nm.pl}
\alias{nm.pl}

\title{Build Commands to Invoke NONMEM}
\description{
 Format a set of commandlines, suitable for submission to a command shell, to
 operate the renamed copy of \sQuote{nm.pl} in an NMQual-mediated NONMEM installation.
}
\usage{
nm.pl(
	command,
	infile,
	outfile=NULL,
	perl='perl',
	checksum=TRUE,
	compile=TRUE, 
	execute=TRUE,
	split=FALSE,
	...
)
}

\arguments{
  \item{command}{path for the installation-specific variant of \file{nm.pl}}
  \item{infile}{path for a control stream}
  \item{outfile}{path for output}
  \item{perl}{how to invoke Perl on this system}
  \item{checksum}{whether to conduct checksums}
  \item{compile}{whether to compile NONMEM}
  \item{execute}{whether to execute the compiled NONMEM}
  \item{split}{whether to compile and execute as separate steps}
  \item{\dots}{ignored}
}
\details{
	If \code{split} is \code{TRUE}, two commands are returned, having the mode flags \sQuote{c} and \sQuote{e}, respectively.
}
\value{character}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item{}{\code{\link{runCommand}}}
	}
}
\examples{
nm.pl('/common/nonmem/nm6/','1.ctl')
nm.pl('/common/nonmem/nm6','1.ctl','1.out',search=FALSE,checksum=FALSE)
nm.pl('/nm6','1.ctl',execute=FALSE)
nm.pl('/nm6','1.ctl',compile=FALSE)
nm.pl('/nm6','1.ctl',split=TRUE)
}
\keyword{manip}

