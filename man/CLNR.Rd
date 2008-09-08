\name{CLNR}
\alias{CLNR}
\title{ Delete files and directories  }
\description{
  CLNR deletes files and directories and logs what was deleted 
  to a text file in the \code{ProjectDir}.
}
\usage{
CLNR(Dir, ProjectDir, note = "Files removed", test = TRUE)
}
\arguments{
  \item{Dir}{ Complete path of directory to be deleted.  This directory
   and all files and sub-directories will be deleted. 
   Example: \code{Dir="/Dir/to/be/Removed"}}
  \item{ProjectDir}{System path to the directory containing the NONMEM control (*.ctl) streams. }
  \item{note}{ character vector (defaults to "Files removed") for user 
  text to describe why something was deleted. Example: \code{note=c("Files removed ")} }
  \item{test}{ logical (defaults to "TRUE") to determine if you want to actually 
  delete the files or just see what files would be deleted if "test=F". To delete directory, files, and
  sub-directories use \code{test=F}}
}
\references{ MIfuns package available from Metrum Institute website (\code{http://metruminstitute.org/}). }
\author{ Developed by Metrum Institute (Bill Knebel, Tim Bergsma, and others). }
\examples{
}
\keyword{ manip }
