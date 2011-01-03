\name{omegacor}
\alias{omegacor}
\title{
Convert NONMEM Omega Covariance Matrix to Correlation Matrix
}
\description{
This function acquires omega covariance matrix elements for a NONMEM 7 run,
and calculates the correlation matrix.  Run and project are used to calculate
the .ext file name, which is read into memory as a pxml object, and then 
converted to unilog format.  Directly supplying extfile, pxml, or unilog renders
all previous arguments moot. Not vectorized: run should be scalar.
}
\usage{
omegacor(
	run=0,
	project=getwd(),
	tool='nm7',
	extfile=file.path(project,run,paste(run,'ext',sep='.')),
	pxml=as.pxml.ext(extfile),
	unilog=as.unilog.pxml(x=pxml,run=run,tool=tool,...),
	...
)
}

\arguments{
  \item{run}{name (number) of a run}
  \item{project}{project directory (parent of run directory)}
  \item{tool}{largely irrelevant; only 'nm7' is supported}
  \item{extfile}{name of .ext file, e.g. run.ext}
  \item{pxml}{internal xml format for parameter data}
  \item{unilog}{unilog format; as returned invisibly by \code{rlog}}
  \item{\dots}{other arguments, unused}
}
\value{
correlation matrix
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item{}{\code{\link{cov2cor}}}
		\item{}{\code{\link{rlog}}}
	}
}
\keyword{manip}

