\name{TruncateParametersForSimulation}
\alias{TruncateParametersForSimulation}
\title{Truncate a set of parameters for simulation}
\description{
 Function that truncates a set of parameters for simulation according to
 a user-defined set a boundaries.
}
\usage{
TruncateParametersForSimulation(parameters, bounds)
}
\arguments{
  \item{parameters}{R dataframe containing the set of parameters that needs to be truncated.}
  \item{bounds}{R dataframe used to assign the parameter bounds. The dataframe should contain
  one row for each parameter that will be truncated and three columns with columns for parameter number (1,2,3, etc. 
  corresponding to the column in /code{parameters} that needs to be truncated), the lower, and upper bound}
}
\details{
  The function returns the \code{parameters} dataframe containing the truncated set of parameters and a message 
  indicating the number of parameters that fell within the user-defined bounds.
}
\references{ MIfuns package available from Metrum Institute website (\code{http://metruminstitute.org/}). }
\author{ Developed by Metrum Institute (Leonid Gibiansky, Bill Knebel, Tim Bergsma, and others). }
\examples{}
\keyword{ manip }
