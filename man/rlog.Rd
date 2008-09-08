\name{rlog}
\alias{rlog}
\title{ Generate a combined run log from multiple NONMEM runs }
\description{
  rlog will generate a combined run log for all run numbers
  requested.  rlog will append to or overwrite any existing
  combined run log depending on the user options specified 
  in the call to rlog.
}
\usage{
rlog(b, boot, ProjectDir, runlog)
}
\arguments{
  \item{b}{ Vector of control stream numbers to use to generate a combined run log.
  Should be numeric with no placeholder zeros to left of number, i.e., "1" not "001".
  Example: \code{b=c(1,2,5)} }
  \item{boot}{ Flag to describe type of NONMEM run with (0)-normal, (1)-bootstrap, 
  (2)-long runs, and (3)-bootstrap runs across entire cluster. 
  Flag should be "0" on Windows. }
  \item{ProjectDir}{System path to the directory containing the NONMEM control (*.ctl) streams. }
  \item{runlog}{ Flag to describe process used to generate the combined run log with
  (0)-no combined run log (0), (1)-generate combined run log of ctl streams in b by 
  appending to existing combined run log, or (2)-generate combined run log of all ctl streams 
  in b by overwriting existing combined run log. }
}
\details{
 
}
\references{ MIfuns package available from Metrum Institute website (\code{http://metruminstitute.org/}). }
\author{ Developed by Metrum Institute (Bill Knebel, Tim Bergsma, and others). }
\examples{}
\keyword{ manip }
