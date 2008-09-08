\name{covplot}
\alias{boxplot.prestats}
\alias{bwplot.distribution}
\title{Make "Forest Plots" of Covariate Effects, etc.}
\description{
bwplot() is customized for a distribution object.  It gives an easy and flexible 
way to construct "Forest Plots", which consist largely of horizontal boxes and 
whiskers that show the effects, for instance, of covariates on model parameters.
}
\usage{
	\method{bwplot}{distribution}(x,data,coef = 1.5, stats = covplot.stats, panel = panel.covplot, prepanel = prepanel.covplot, transform = "x*1", pch = 1, xlab = NULL, ref = NULL, ...)
	\method{boxplot}{prestats}(x, coef, ...)
}
\arguments{
  \item{x}{For bwplot.distribution, a formula. for boxplot.prestats, numeric.}
  \item{data}{A distribution object.}
  \item{coef}{Like coef in boxplot.stats.}
  \item{stats}{A function to compute boxes and whiskers for data subsets.}
  \item{panel}{A function to plot each panel in a conditioned Lattice plot.}
  \item{prepanel}{A function to set up the limits of the plot.}
  \item{transform}{Character equivalent of an expression to rework the continuous
  axis in a Forest Plot.}
  \item{pch}{The symbol used for plotting in a bwplot.}
  \item{xlab}{Label for the x-axis.}
  \item{ref}{Where to put the reference line in native units of \code{x}.}
  \item{\dots}{Extra arguments passed to called functions.}
}
\details{
bwplot(), when called with a distribution object in the first position, dispatches
bwplot.distribution().  This is a specialization of bwplot that constructs 
"Forest Plots" or "Covariate Plots". 

The distribution object typically consists of columns of bootstrap estimates of 
model parameters.  Each column has extra attributes associated with it, including
at least "z" and "influence".  bwplot.distribution() calls 'stats' indirectly:
boxplot.prestats() is called first, which unpacks and passes to 'stats' any extra
arguments not normally passed to 'stats'.  By default, 'stats' is covplot.stats,
which expects as extra arguments "z" and "influence".

The basic structure of the plot is controlled by the formula argument \code{x}. 
It has the form "A~A + B + C...".  Typically, A is a covariate of interest. Inclusion
of A on the left hand side of the formula operator makes the median of the 
column named "A" in the distribution object available as "A" to authors of 
influence expressions (submodels); it also makes "A" the default x axis label.
Inclusion of A on the right hand side of the formula operator causes the 
corresponding column to be plotted, as per its z and influence arguments, but 
with a special color.  Arguments on the right hand side cause corresponding bw 
elements to be plotted in top-down order.  Many features of the plot can be 
customized by passing extra arguments expected by panel.covplot() or panel.bwplot().
}
\value{
bwplot.distribution() returns a trellis object. boxplot.prestats() returns whatever
is returned by stats.
}
\author{Tim Bergsma}
\note{
The only two functions the user calls directly are \code{as.distribution()} 
(called with a data.frame) and \code{bwplot()} (called with a distribution 
object as first argument). But see in particular extra arguments passable to 
panel.covplot().
}
\seealso{
	\code{\link{as.distribution}},
	\code{\link{panel.covplot}}
	\code{\link{covplot.stats}}
	\code{\link{bwplot}}
}
\examples{
cov <- data.frame(			#covariates
	WT=70 + rnorm(50)*10,		#weight, with some variance
	AGE=45+rnorm(50)*20,		#age, with some variance
	MALE=rep(c(TRUE,FALSE),50)	#half males, half females
)

boot <- data.frame(			#bootstrap estimates
	CL=40 + rnorm(100)*5,		#parameter estimate for clearance
	WT=rnorm(100),			#hovers around zero: no effect
	AGE=rnorm(
		100,
		mean=1,			#hovers around 1: cl prop. to age
		sd=0.5			#estimated imprecisely
	),		
	MALE=rnorm(			#categorical effect
		100,					
		mean=1.15,		#males have 15% greater clearance
		sd=0.05			#estimated very precisely
	)
)
	
z.influence <- list(			#encode side-by-side for sanity
	1, "x",                  	#CL 
	cov$WT, "CL * (z/70)^x",   	#WT ~ normalized WT to power: est.
	cov$AGE, "CL * (z/45)^x",  	#AGE ~ normalized AGE to power: est.
	1, "CL * x^z"			#MALE ~ est. to power: MALE
)
library(lattice)
boot <- as.distribution(
	boot,
	z=z.influence[c(TRUE,FALSE)],		#(just the odd elements)
	influence=z.influence[c(FALSE,TRUE)]	#(just the even elements)
)

bwplot(CL~CL+AGE+WT+MALE,boot,rlim=0.2)
}

\keyword{manip}

