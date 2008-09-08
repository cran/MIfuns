\name{Tmin}
\alias{Tmin}
\title{Calculate Tmin}
\description{
 Calculate the time associated with the minimum concentration for each subject
}
\usage{
Tmin(data, id = "ID", dv = "DV", time = "TIME")
}
\arguments{
  \item{data}{the name of the R data.frame containing the data to use for the Tmin calculation.}
  \item{time}{chronologically ordered time variable present in \code{data}}
  \item{id}{variable in \code{data} defining subject level data}
  \item{dv}{dependent variable used to calculate Tmin present in \code{data}}	
}
\details{
The Tmin() function performs the calculation based on the variables id, time, and dv present in the R data object
}
\value{
  one Tmin is returned for each subject }
\references{ MIfuns package available from Metrum Institute website (\code{http://metruminstitute.org/}). }
\author{ Developed by Metrum Institute (Bill Knebel, Tim Bergsma, and others). }
\examples{}
\keyword{ manip }
