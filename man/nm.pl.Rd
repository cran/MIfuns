\name{nm.pl}
\alias{nm.pl}
\alias{autolog.pl}
\title{Build Commands to Invoke NONMEM}
\description{
 Format a set of commandlines, suitable for submission to a command shell, to
 operate the executable for an NMQual-mediated NONMEM installation.
}
\usage{
nm.pl(
	command,
	ctlfile,
	outfile=NULL,
	perl='perl',
	checksum=TRUE,
	compile=TRUE, 
	execute=TRUE,
	split=FALSE,
	...
)
autolog.pl(
	command, 
	rdir, 
	run, 
	perl = "perl", 
	compile = TRUE, 
	execute = TRUE, 
	split = FALSE, 
	mode = "run", 
	...
)
}

\arguments{
  \item{command}{path for the installation-specific variant of \file{nm.pl}}
  \item{ctlfile}{path for a control stream}
  \item{outfile}{path for output}
  \item{perl}{how to invoke Perl on this system}
  \item{checksum}{whether to conduct checksums}
  \item{compile}{whether to compile NONMEM}
  \item{execute}{whether to execute the compiled NONMEM}
  \item{split}{whether to compile and execute as separate steps}
  \item{rdir}{run directory}
  \item{run}{run name}
  \item{mode}{process mode}
  \item{\dots}{ignored}
}
\details{
	If \code{split} is \code{TRUE}, two commands are returned, having the mode flags \sQuote{c} and \sQuote{e}, respectively.
	nm.pl drops the 'c' or 'e' argument if \code{split} is FALSE and
	both \code{compile} and \code{execute} are TRUE.  In the identical case,
	autolog.pl passes 'ce'. autolog.pl also returns a run directory and run
	name, rather than input and output file paths.  autolog.pl does not 
	support \code{checksum}.
}
\value{character}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item \code{\link{runCommand}}
	}
}
\examples{
nm.pl('/nm6/test/nm6.pl','1.ctl')
nm.pl('/nm6/test/nm6.pl','1.ctl','1.out',checksum=FALSE)
nm.pl('/nm6/test/nm6.pl','1.ctl',execute=FALSE)
nm.pl('/nm6/test/nm6.pl','1.ctl',compile=FALSE)
nm.pl('/nm6/test/nm6.pl','1.ctl',split=TRUE)
autolog.pl('/nm72/nmqual/autolog.pl','/home/ubuntu/test/3',3)
}
\keyword{manip}

