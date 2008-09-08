\name{riwish}
\alias{riwish}
\title{Generate inverse Wishart distribution}
\description{
 Generates an inverse Wishart distribution for a variance of a random effect (OMEGA matrix in NONMEM notation). 
 The functions requires the specification of degrees of freedom, scale matrix, and precision for each random effect.
}
\usage{
riwish(s, df, Prec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s}{scale matrix for inverse Wishart distribution}
  \item{df}{degrees of freedom}
  \item{Prec}{presision matrix with the mean of riwish() proportion to the \code{Prec}}
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
 The variance of the random effect can be drawn form the returned inverse Wishart distribution.
 }
\references{ MIfuns package available from Metrum Institute website (\code{http://metruminstitute.org/}). }
\author{ Developed by Metrum Institute (Leonid Gibiansky, Bill Knebel, Tim Bergsma, and others). }
\examples{}
\keyword{ manip }
