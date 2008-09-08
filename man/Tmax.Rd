\name{Tmax}
\alias{Tmax}
\title{Calculate Tmax}
\description{
 Calculate the time associated with the maximum concentration for each subject
}
\usage{
Tmax(data, id = "ID", dv = "DV", time = "TIME")
}
\arguments{
  \item{data}{the name of the R data.frame containing the data to use for the Tmax calculation.}
  \item{time}{chronologically ordered time variable present in \code{data}}
  \item{id}{variable in \code{data} defining subject level data}
  \item{dv}{dependent variable used to calculate Tmax present in \code{data}}	
}
\details{
The Tmax() function performs the calculation based on the variables id, time, and dv present in the R data object
}
\value{
  one Tmax is returned for each subject }
\references{ MIfuns package available from Metrum Institute website (\code{http://metruminstitute.org/}). }
\author{ Developed by Metrum Institute (Bill Knebel, Tim Bergsma, and others). }
\examples{}
\keyword{ manip }
