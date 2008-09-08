\name{rinvgamma}
\alias{rinvgamma}
\title{Generate inverse gamma distribution}
\description{
  Generate inverse gamma distribution given n, df, and sigma estimate
}
\usage{
rinvgamma(n, df, sigma)
}
\arguments{
  \item{n}{the number of values to return}
  \item{df}{the degrees of freedom for the distribution. This value reflects the uncertainity in the distribution. A reasonable starting estimate would be a value between the number of patients and the number of observations used to obtain the sigma estimates}
  \item{sigma}{vector of sigma estimates}
}
\details{
}
\value{
 The \code{rinvgamma} function returns \code{n} sigma estimates for use as simulation parameters.
}
\references{ MIfuns package available from Metrum Institute website (\code{http://metruminstitute.org/}). }
\author{ Developed by Metrum Institute (Bill Knebel, Tim Bergsma, and others). }
\examples{}
\keyword{utilities}


