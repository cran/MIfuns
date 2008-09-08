\name{AUC}
\alias{AUC}
\title{Calculate AUC using the trapezoidal method}
\description{
  Calculate the area under the curve (AUC) for each subject over the time interval for
  dv using the trapezoidal rule. 
  }
\usage{
AUC(data, time = "TIME", id = "ID", dv = "DV")
}
\arguments{
  \item{data}{the name of the R dataframe containing the data to use for the AUC calculation.}
  \item{time}{chronologically ordered time variable present in \code{data}}
  \item{id}{variable in \code{data} defining subject level data}
  \item{dv}{dependent variable used to calculate AUC present in \code{data}}
}
\details{
  The AUC() function performs the calculation based on the variables id, time, and dv present in the R data object
}
\value{
  one area under the dv-time curve is returned for each subject 
}
\references{ MIfun package available from Metrum Institute website (\code{http://metruminstitute.org/}). }
\author{ Developed by Metrum Institute (Bill Knebel, Tim Bergsma, and others). }
\examples{}
\keyword{ manip }
