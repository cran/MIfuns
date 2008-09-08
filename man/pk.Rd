\name{pk}
\alias{acr}
\alias{cavg}
\alias{cmax}
\alias{cmin}
\alias{css}
\alias{ke}
\alias{tmax}
\alias{auc}
\title{Calculate One-Compartment Model Properties (beta)}
\description{
These functions calculate steady-state properties from the parameters of a 
one-compartment pharmacokinetic model, assuming all doses occur in the post-
distribution phase (tau >>> tmax).
}
\usage{
acr(cl, v, tau, ...)
cavg(cl, tau, dose, ...)
cmax(cl, v, ka, tau, dose, ...)
cmin(cl, v, ka, tau, dose, ...)
css(cl, v, ka, tau, dose, time, ...)
ke(cl, v, ...)
tmax(cl, v, ka, tau, ...)
auc(cl, dose, ...)
}
\arguments{
  \item{cl}{Apparent clearance (L/h)}
  \item{v}{Apparent volume (L)}
  \item{ka}{Absorption constant (h\^-1)}
  \item{tau}{Steady-state trough-to-trough interval (h)}
  \item{dose}{Dose (arbitrary units)}
  \item{time}{Arbitrary time point, <= tau}
  \item{\dots}{other arguments, ignored}
}
\details{
acr() calculates accumulation ratio (unitless, called by css()).
ke() calculates the elimination constant (h\^-1, called by css()).
css() calculates steady-state compartment concentration (dose units per L) 
at a given time.
cmin() and cmax() calculate minimum and maximum steady-state concentrations; they
call css(), passing either tau itself or tmax for the time argument.
tmax() calculates the time of the maximum concentration.
auc() calculates area under the curve.

Note that for the post-distributive assumption to hold, tau must be very large
relative to the time needed for drug disposition.  To the extend that it does not
hold, these functions give biased results.
}
\value{
Numeric.
}
\references{Gibaldi M, Perrier D. Multiple dosing. Pharmacokinetics. 
	New York: Marcel Dekker, Inc., 1982: p. 113- 144. }
\author{Tim Bergsma}
\note{
Multiple properties can be calculated within the same function or loop, since 
all arguments can be passed even if not needed.  For example, the same five
arguments can be passed, even un-named, to cmax and tmax (tmax will ignore dose).  
But be sure to pass by name the same arguments to cmax and cavg: the latter will
ignore unused arguments but has the common arguments in a different order.
}
\examples{
cavg(cl=0.05,tau=300,dose=100)
cmax(cl=0.05,v=10,ka=0.015,tau=300,dose=100)
cmin(cl=0.05,v=10,ka=0.015,tau=300,dose=100)
tmax(cl=0.05,v=10,ka=0.015,tau=300)
}

\keyword{manip}

