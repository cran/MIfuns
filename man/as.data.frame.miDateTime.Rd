\name{as.data.frame.miDateTime}
\alias{as.data.frame.miDateTime}
\title{Create a data.frame from miDateTime}
\description{Create a data.frame from miDateTime}
\usage{
\method{as.data.frame}{miDateTime}(x, row.names = NULL, optional = FALSE, ...)
}
\arguments{
  \item{x}{miDateTime}
  \item{row.names}{row names for the result}
  \item{optional}{Optional to use the object name as the column name?}
  \item{\dots}{ignored arguments}
}
\details{
  Utility for combining miDateTime objects into data.frames.
}
\value{
data.frame
}
\references{http://metruminstitute.org}
\author{Tim Bergsma}
\seealso{
\code{\link{as.miDateTime}},
\code{\link{as.data.frame}}}
\examples{
function (x, row.names = NULL, optional = FALSE, ...) 
{
    nrows <- length(x)
    nm <- paste(deparse(substitute(x), width.cutoff = 500), collapse = " ")
    if (is.null(row.names)) {
        if (nrows == 0) 
            row.names <- character(0)
        else if (length(row.names <- names(x)) == nrows && !any(duplicated(row.names))) {
       }
        else row.names <- .set_row_names(nrows)
   }
    names(x) <- NULL
    value <- list(x)
    if (!optional) 
        names(value) <- nm
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    value
 }
}
\keyword{manip}
