\name{as.unilog.run}
\alias{as.unilog.run}
\title{Create a Run Log for NONMEM7 using NONMEM6 Format}
\description{
  MIfuns includes an INFN routine for NONMEM6 and earlier that creates a 
  comma-separated listing of parameter estimates and their percent relative
  standard errors.  This function emulates that data, using the \file{.ext} file
  produced under NONMEM7. \code{as.runlog.unilog} converts the output to the
  format used for NONMEM6.
}
\usage{
as.unilog.run(
	run,
	outfile=paste(run,'lst',sep='.'),
	extfile=file.path(
		dirname(outfile),
		paste(run,'ext',sep='.')
	),
	tool='nm7',
	...
)
}
\arguments{
  \item{run}{name of the NONMEM run, typically integer}
  \item{outfile}{path for the NONMEM output file, typically \file{run.lst}}
  \item{extfile}{path for the NONMEM estimates, by default \file{run.ext}}
  \item{tool}{currently \sQuote{nm7}}
  \item{\dots}{passed to other functions}
}
\details{
	The \sQuote{ext} file is digested.  In particular the rows labeled -1000000000
  and -1000000001 are taken as the parameter estimates and standard errors,
  respectively.  The minimization status is either 0 or 1, which is less 
  informative than for NONMEM6.  The covariance status is not currently
  represented.
}
\value{
  data.frame with columns \emph{tool, run, parameter, moment, value}.
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item{}{\code{\link{as.unilog.lst}}}
		\item{}{\code{\link{as.unilog.pxml}}}
		\item{}{\code{\link{as.runlog.unilog}}}
	}
}
\keyword{manip}

