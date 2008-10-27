\name{c.miTemporal}
\alias{c.miTemporal}
\alias{[.miTemporal}
\alias{[<-.miTemporal}
\alias{[[.miTemporal}
\alias{as.character.miTemporal}
\alias{as.chartime.numeric}
\alias{as.chartime}
\alias{as.numeric.chartime}
\alias{print.miTemporal}
\alias{seq.miTemporal}
\alias{subclass}


\title{ Support for miTemporal Classes }
\description{
  These functions support classes miTemporal, miTime, miDate, and miDateTime
  (and related functions).  They are mostly S3 methods for base R generics.
}
\usage{
\method{c}{miTemporal}(..., recursive = FALSE)
\method{[}{miTemporal}(x, ..., drop = TRUE)
\method{[}{miTemporal}(x, ...) <- value
\method{[[}{miTemporal}(x, ..., drop = TRUE)
\method{as.character}{miTemporal}(x, ...)
as.chartime(x, ...)
\method{as.chartime}{numeric}(x, format, mark=TRUE,...)
\method{as.numeric}{chartime}(x, format,...)
\method{print}{miTemporal}(x, ...)
\method{seq}{miTemporal}(from, to, by = NULL, length.out = NULL, along.with = NULL, ...)
subclass(x, class,...)
}

\arguments{
  \item{\dots}{ arguments to c(), or passed to other functions }
  \item{recursive}{ same meaning as for c() }
  \item{x}{ object of class miTemporal }
  \item{drop}{ same meaning as for '[' and '[[' }
  \item{value}{ value to be assigned, as for '[<-' }
  \item{format}{ input or output format describing character time (see \code{\link{strftime}} }
  \item{mark}{boolean: mark times with dangling seconds using '+'}
  \item{from}{ as for \code{\link{seq.default}} }
  \item{to}{ as for \code{\link{seq.default}} }
  \item{by}{ as for \code{\link{seq.default}} }
  \item{length.out}{ as for \code{\link{seq.default}} }
  \item{along.with}{ as for \code{\link{seq.default}} }
  \item{class}{ class to prepend to class attribute of x }
}
\details{
  Normally you shouldn't need to worry about these functions.  c() and the '[' 
  variants exist just so that class information is not lost on invocation of the 
  generic.  as.character.miTemporal() and print.miTemporal() just call format().
  'chartime' variants are used internally by other functions.  seq.miTemporal() 
  requires 'from' and 'to', sets smart defaults for 'by' (if null), and preserves
  class information, relative to seq.default(). subclass() returns its first 
  argument with 'class' prepended to its class list.
}
\value{
  \item{subclass}{see above}
  \item{print}{an invisible object with same class as x}
  \item{as.chartime}{generic: does not return}
  \item{as.chartime.numeric}{character (time)}
  \item{as.numeric.chartime}{numeric (seconds)}
  \item{as.character.miTemporal}{character (time)}
  \item{others}{object with same class as x}
}
\references{ http://mifuns.googlecode.com }
\author{ Tim Bergsma }
\seealso{ 
	\code{\link{miTemporal}},
	\code{\link{seq.default}},
	\code{\link{strftime}},
}
\examples{

#as.data.frame
data.frame(
	dt=as.miDateTime(seq(from=0,by=86500,length.out=3)),
	d=as.miDate(seq(from=0,by=86400,length.out=3)),
	t=as.miTime(c(60,120,180))
)
#                 dt          d     t
#1  01/01/1970 00:00 01/01/1970 00:01
#2 01/02/1970 00:01+ 01/02/1970 00:02
#3 01/03/1970 00:03+ 01/03/1970 00:03

#combine
c(as.miTime(0),as.miTime(60))
# 00:00 00:01
c(as.miDate(0),as.miDate(86400))
# 01/01/1970 01/02/1970
c(as.miDateTime(0),as.miDateTime(86500))
# 01/01/1970 00:00  01/02/1970 00:01+

#subset
as.miTime(c("08:00","09:00"))[2]
# 09:00
as.miDate(c("2008-01-01","2008-01-04"))[2]
# 01/04/2008
as.miDateTime(c("2008-01-01 12:00","2008-01-04 12:30"))[2]
# 01/04/2008 12:30

#element selection
as.miTime(c("08:00","09:00"))[[2]]
# 09:00
as.miDate(c("2008-01-01","2008-01-04"))[[2]]
# 01/04/2008
as.miDateTime(c("2008-01-01 12:00","2008-01-04 12:30"))[[2]]
# 01/04/2008 12:30

#assignment
a <- as.miTime(seq(60,300, by=60))
a#00:01 00:02 00:03 00:04 00:05
a[5] <- 60
a#00:01 00:02 00:03 00:04 00:01
a[3] <- NA
a#00:01 00:02 NA    00:04 00:01

#identity
as.miTime(as.miTime(0))
# 00:00
as.miDate(as.miDate(0))
# 01/01/1970
as.miDateTime(as.miDateTime(0))
# 01/01/1970 00:00
}
\keyword{ manip }
