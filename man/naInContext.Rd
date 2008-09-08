\name{naInContext}
\alias{naInContext}
\title{ Display Missing Values in Context }
\description{
  Display rows of data with missing values, as well as other rows with the same
  key.
}
\usage{
naInContext(x, context, search)
}
\arguments{
  \item{x}{A data.frame.}
  \item{context}{Character vector of column names representing a key.}
  \item{search}{Character vector of column names representing search fields.}
}
\details{
  Often one wants to view not just records with missing values, but
  other
  related records as well, e.g. "view all the records for any subject
  with
  missing doses".  \code{naInContext} searches for NAs in columns of x
  named by \code{search}.  It then determines keys for these rows, using
  columns in x named by \code{context}.  Finally, it displays all rows
  with matching keys.
}

\value{
  A data.frame.
}
\author{ Tim Bergsma}

\seealso{  \code{\link{help}},  }
\examples{
test <- Theoph
test$conc[[5]] <- NA
naInContext(test, context="Subject", search="conc")
}
\keyword{ manip }

