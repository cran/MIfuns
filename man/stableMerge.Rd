\name{stableMerge}
\alias{stableMerge}
\title{ Execute a Benign Left Join }
\description{
  Merge Two Data Sets Without Altering Material from the Left Argument.
}
\usage{
stableMerge(x, y)
}
\arguments{
  \item{x}{The main data.frame.}
  \item{y}{A second data.frame containing related information}
}
\details{
	\code{merge} is notoriously unpredictable, because it can add, drop, and
	reorder rows, as well as reorder columns and recreate row names. 
	\code{stableMerge} solves all that.  It expects a primary data frame and
	a secondary dataframe, and performs a 'stable' left join (all.x=TRUE, 
	(all.y=FALSE). Absence of extra arguments is by design, for integrity.
	Keys are defined by common columns, and are repeatable in the primary, 
	but must be unique in the secondary dataframe. New columns in the 
	secondary frame are appended to the primary, supplying secondary info 
	by key match.  No rows are added, or dropped, row/column order is 
	unaffected, and row names are preserved.
}
\value{
  A dataframe with as many rows as nrow(x) but possibly more columns.
}
\author{Tim Bergsma}

\seealso{  \code{\link{merge}}  }
\examples{
#a nonsense example
stableMerge(Theoph,BOD)
}
\keyword{ manip }

