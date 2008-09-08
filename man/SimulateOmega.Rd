\name{SimulateOmega}
\alias{SimulateOmega}
\alias{myriwish}
\title{Generate omega values for simulation}
\description{
  Generate omega values given the number of simulations, omega degrees of freedom, and
  list containing omega block values
}
\usage{
SimulateOmega(nsim, df, Cov)
}
\arguments{
  \item{nsim}{the number of simulations that will be performed}
  \item{df}{the degrees of freedom of each omega block element}
  \item{Cov}{list containing the elements for each omega block element with one omega block per list item}
}
\details{
  The number of elements in the \code{df} list and \code{Cov} list should be equal. Functions 
  \code{riwish} and \code{myriwish} are called by \code{SImulateOmega}. 
}
\value{
 One omega value is returned for each block element. 
}
\references{ MIfuns package available from Metrum Institute website (\code{http://metruminstitute.org/}). }
\author{ Developed by Metrum Institute (Bill Knebel, Tim Bergsma, and others). }
\seealso{\code{\link{CreateParametersForSimulation}} }
\examples{}
\keyword{ manip }
