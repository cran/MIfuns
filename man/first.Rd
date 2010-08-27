\name{first}
\alias{first}
\alias{nth}
\alias{last}
\alias{distance}
\alias{before}
\alias{at}
\alias{after}

\title{Identify Elements Within Cells Meeting Some Instance of a Condition}
\description{
	Within each level of \code{within}, find the \eqn{n^{th}}{n^th} element for which
	\code{where} is \code{TRUE}, and repeat it at all positions in the level.
}
\usage{
first(x,where=rep(TRUE,length(x)),within=rep(1,length(x)),...)
last (x,where=rep(TRUE,length(x)),within=rep(1,length(x)),n=-1,...)
nth  (x,where=rep(TRUE,length(x)),within=rep(1,length(x)),n=1,...)
distance(where,within=rep(1,length(where)),n=1,...)
before(  where,within=rep(1,length(where)),n=1,...)
at(      where,within=rep(1,length(where)),n=1,...)
after(   where,within=rep(1,length(where)),n=1,...)
}
\arguments{
  \item{x}{vector}
  \item{where}{vector, coerced to logical}
  \item{within}{a vector or list of vectors serving as an index}
  \item{n}{an integer giving the instance of interest}
  \item{\dots}{ignored arguments.}

}
\details{
  \code{first} returns, for each position in \code{x}, the element corresponding to 
  the first instance of \code{TRUE} in \code{where}, optionally nested within the index (\code{within}).
  
  \code{nth} is a generalization operating on the \eqn{n^{th}}{n^th} instance, rather than the first.  
  \code{n} can be 0, returning all \code{NA}, or negative,
  which counts instances of \code{TRUE} from the end of \code{where}.
  
  One of \code{x} or \code{where} must be specified.  If \code{x} is missing, positions of 
  elements in \code{where} are used as \code{x}.  \code{where} by default is an all-\code{TRUE} vector as long
  as \code{x}.  If \code{where} is shorter than \code{x}, it is replicated to length of \code{x} with
  warning.
  
  \code{last} is a convenience wrapper for \code{nth} that silently coerces \code{n} to -1.
  
  \code{distance} merely subtracts, from positions in \code{where}, the positions corresponding to \eqn{n^{th}}{n^th} \code{TRUE} 
  (optionally within levels of \code{within}). \code{before}, \code{at}, and \code{after} test
  whether distance is less than, equal to, or greater than zero.
}
\value{
	vector of same class as \code{x}, if supplied, else of class integer (or logical for \code{before},
	\code{at}, and \code{after}).
}
\references{\url{http://mifuns.googlecode.com}}
\author{Tim Bergsma}
\seealso{
	\itemize{
		\item{}{\code{\link{match}}}
	}
}

\examples{
first(1:10)
first(1:10,within=rep(1:2,each=5))
last( 1:10,within=rep(1:2,each=5))
first(1:10,where=0:1,within=rep(1:2,each=5))
test <- data.frame(
	id=c(1,1,1,1,1,2,2,2,2,2),
	time=c(1,2,3,4,5,1,2,3,4,5),
	evid=c(0,1,0,1,0,1,0,1,0,1)
)
test$secondDose  <- with(test,nth(where=evid==1, n=2,within=id))
test$lastDose    <- with(test,nth(where=evid==1,n=-1,within=id))
test$beforeDose2 <- with(test,before(where=evid==1, n=2,within=id))
test$tafd        <- with(test,time - first(time,where=evid==1,within=id))#time after first dose
test
#    id time evid secondDose lastDose beforeDose2 tafd
# 1   1    1    0          4        4        TRUE   -1
# 2   1    2    1          4        4        TRUE    0
# 3   1    3    0          4        4        TRUE    1
# 4   1    4    1          4        4       FALSE    2
# 5   1    5    0          4        4       FALSE    3
# 6   2    1    1          8       10        TRUE    0
# 7   2    2    0          8       10        TRUE    1
# 8   2    3    1          8       10       FALSE    2
# 9   2    4    0          8       10       FALSE    3
# 10  2    5    1          8       10       FALSE    4
}
\keyword{manip}

