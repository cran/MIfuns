\name{CreateParametersForSimulation}
\alias{CreateParametersForSimulation}
\title{Generate model parameters for simulation}
\description{
 Generate \code{nsim} sets of model parameters for simulation based on the
 results of a (final) model
}
\usage{
CreateParametersForSimulation(nsim, ThetaMean, ThetaCovar, OmegaModeList, OmegaDfList, SigmaModeList, SigmaDfList, digits = 4)
}
\arguments{
  \item{nsim}{the number of simulations/sets of model parameters}
  \item{ThetaMean}{vector containing the mean values of the fixed-effect parameters}
  \item{ThetaCovar}{variance-covariance matrix of the fixed-effect distributions}
  \item{OmegaModeList}{List with elements specifying the blocks of the block-diagonal 
  variance-covariance matrix of the random effects. Each element of the list is the data 
  frame that specifies the central values of the variance-covariance block.}
  \item{OmegaDfList}{Vector that specifies degrees of freedoms for each block of the 
  variance-covariance matrix of the random effects. Number of elements in \code{OmegaModeList}
  list and \code{OmegaDfList} vector should be equal.}
  \item{SigmaModeList}{List with elements specifying the blocks of the block-diagonal 
  variance-covariance matrix of the residual errors. Each element of the list is the data 
  frame that specifiesthe central values of the variance-covariance block.}
  \item{SigmaDfList}{Vector that specifies the degrees of freedoms for each block of the 
  variance-covariance matrix of the residual errors. Number of elements in \code{SigmaModeList} and
  \code{SigmaDfList} vector should be equal. }
  \item{digits}{number of significant digits in the output (the default value is 4)}
}
\details{
 
}
\value{
  The output of the function is the data frame that contains the parameter data set. }
\references{ MIfuns package available from Metrum Institute website (\code{http://metruminstitute.org/}). }
\author{ Developed by Metrum Institute (Bill Knebel, Tim Bergsma, and others). }
\seealso{ }
\examples{
}
\keyword{ manip }

