\name{impute}
\alias{forbak}
\alias{locf}
\alias{nocb}
\title{Impute Missing Vector Values}
\description{
Impute NA values using other values in the vector.
}
\usage{
locf(x)
forbak(x)
nocb(x)
}
\arguments{
  \item{x}{A vector with some missing values and some non-missing values.}
}
\details{
locf() implements "last observation carried forward":  NA's are imputed with 
the most recent non-NA value.  nocb() is the complement: "next observation 
carried backward":  NA's are imputed with the next non-NA value.  forbak()
first executes locf(), then nocb(), so that even leading NAs are imputed. 
If even one non-NA value is present, forbak() should not return any NAs.
}
\value{
A vector of the same class as x.
}
\author{Tim Bergsma}
\examples{
locf(c(NA,1,2,NA,NA,3,NA,4,NA))
nocb(c(NA,1,2,NA,NA,3,NA,4,NA))
forbak(c(NA,1,2,NA,NA,3,NA,4,NA))
}

\keyword{manip}

