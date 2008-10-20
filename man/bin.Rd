\name{bin}
\alias{bin}
\title{ Calculate Bin Sizes and Limits for a Vector (beta)}
\description{
Given a numeric vector, calculate bin limits, place each value in a bin, and
return the number of values in each bin.
}
\usage{
bin(x, population = x, breaks = quantile(population,
                                probs = c(0, 0.25, 0.5, 0.75, 1), ...), ...)
}
\arguments{
  \item{x}{A vector of numeric values, to be placed in bins.}
  \item{population}{A vector of numeric values serving as the reference
  population for constructing bins.}
  \item{breaks}{Bin limits (boundaries) to pass to cut().}
  \item{\dots}{Other arguments to pass to quantile(), table(), or cut().}
}
\details{
By default, the population used to calculate bin limits is the same as the
group of values being binned.  By default, inner bin limits are the quartiles
of the population.
}
\value{
A table with bin limits encoded as column names.
}
\author{Tim Bergsma}
\seealso{\code{\link{quantile}}, \code{\link{table}}, \code{\link{cut}}}
\examples{
bin(1:100)
bin(1:50,population=1:100)
plot(
    bin(
    	rnorm(1000), 
	breaks=seq(
		from=-3,
		to=3,
		by=0.5
	)
    )
)
}

\keyword{manip}

