\name{as.distribution}
\alias{as.annotated.data.frame}
\alias{as.annotated}
\alias{as.distribution.data.frame}
\alias{as.distribution}
\alias{[.annotated}
\alias{[[.annotated}
\alias{print.annotated}

\title{Make Specialized Data Frames to Support "Forest Plots"}
\description{
as.annotated() and as.distribution() are generic, with methods defined for
data.frame.
	
Class "annotated" is a subclass of "data.frame".  It has arbitrary "annotations"
which consist of named lists (or vectors) having one descriptive element for 
each column in the data.frame.  Class "distribution" is a subclass of "annotated",
having at least "z" and "influence" among its annotations.  "z" and "influence" 
are required by covplot.stats(), the default stats function for bwplot.distribution().
"annotated" methods are defined for element selection, subsetting, and printing.
}
\usage{
	\method{as.annotated}{data.frame}(x, ...)
	\method{as.distribution}{data.frame}(x, z, influence, ...)
	\method{[}{annotated}(x,...)
	\method{[[}{annotated}(x,...)
	\method{print}{annotated}(x,...)
}
\arguments{
  \item{x}{The argument on which dispatching occurs, typically a data.frame.}
  \item{z}{A list or vector of numerics, with elements for each column in x, 
	  giving the value(s) of the covariate at which to evaluate the 
  	  corresponding submodel.}
  \item{influence}{A list or vector of mode character, with elements for each 
	  column in x, giving the corresponding submodels.}
  \item{\dots}{Extra named arguments, each having as many elements as columns in
  		x, giving further annotations that may be referenced in submodels.}
}
\details{
bwplot.distribution() wants to make plot elements, per column specified, that take
into account the distribution of the column values, typically parameter estimates
of a covariate effect (x). Also to be considered are the value of a model parameter
on which the covariate has an effect (y), and the actual values of the covariate
that were used in estimating the parameter(z).  These are integrated by a submodel,
supplied on a per column basis as the 'influence' argument.

For a given parameter distribution, the corresponding element of \code{z} should 
be the values of the covariate at which the submodel should be evaluated.  Typically
this is the value 1 for binary (categorical) covariates, and the set of original 
covariate values for continuous covariates.

For a given parameter distribution, the corresponding element of \code{influence}
should be a text representation of an R expression for the covariate submodel.  
Authors have at their disposal the (column) name of the primary model parameter,
the symbol x which represents the distributed parameter, and the symbol z which
represents the corresponding element of \code{z}.

Additionally submodel authors have at their disposal the name of any other annotation
which which the distribution object was constructed.  For annotations that are
not column-wise atomic, length should probably be the same as x, or some factor
thereof. Exactly what happens to non-atomic annotations is highly specific to the 
stats function (covplot.stats() by default).
}
\value{
as.distribution() returns a distribution object.  bwplot.distribution() returns a
trellis object.
}
\author{Tim Bergsma}
\seealso{
	\code{\link{bwplot.distribution}},
	\code{\link{panel.covplot}}
	\code{\link{covplot.stats}}
	\code{\link{bwplot}}
}
\keyword{manip}

