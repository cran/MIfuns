\name{miTemporal}
\alias{miTemporal}
\alias{as.miTime}
\alias{as.miTime.character}
\alias{as.miTime.numeric}
\alias{as.miTime.miTime}
\alias{as.miDate.character}
\alias{as.miDate}
\alias{as.miDate.numeric}
\alias{as.miDate.miDate}
\alias{as.miDateTime}
\alias{as.miDateTime.character}
\alias{as.miDateTime.numeric}
\alias{as.miDateTime.miDate}
\alias{as.miDateTime.miDateTime}
\alias{format.miTime}
\alias{format.miDate}
\alias{format.miDateTime}


\title{Temporal Classes with Selective Defaults}
\description{
  \code{miTemporal} is an abstract superclass of \code{miTime}, \code{miDate},
  and \code{miDateTime}.  These latter are convenience classes that store temporal
  information as seconds since the start of 1970-01-01.  They rely on POSIXlt and
  POSIXct, giving full access to the format constructs of \code{\link{strftime}}.
  However, the concepts of 'origin' and 'timezone' are deconstructed (fixed to 
  1970-01-01 and GMT).  Default formats are suitably chosen for inputs 
  ('as.character' methods) and outputs ('format' methods) and may be overridden. 
  By default, format() will append a '+' symbol to temporals with dangling seconds 
  (not multiples of60): seconds are not displayed by default but still operate 
  (perhaps dangerously) in comparisons.
}
\usage{
as.miTime(x, ...)
\method{as.miTime}{character}(x, format = "\%H:\%M",...)
\method{as.miTime}{numeric}(x,...)
\method{as.miTime}{miTime}(x, ...)
as.miDate(x, ...)
\method{as.miDate}{character}(x, format = "\%Y-\%m-\%d",...)
\method{as.miDate}{numeric}(x,...)
as.miDateTime(x, ...)
\method{as.miDateTime}{character}(x, format = "\%Y-\%m-\%d \%H:\%M",...)
\method{as.miDateTime}{numeric}(x,...)
\method{as.miDateTime}{miDate}(x, y = 0,...)
\method{as.miDateTime}{miDateTime}(x, ...)
\method{format}{miTime}(x, format = "\%H:\%M", mark=TRUE,...)
\method{format}{miDate}(x, format = "\%m/\%d/\%Y", mark=TRUE,...)
\method{format}{miDateTime}(x, format = "\%m/\%d/\%Y \%H:\%M", mark=TRUE,...)
}

\arguments{
  \item{x}{ character time as per format, numeric seconds since 1970-01-01, 
  	or miTemporal subclass}
  \item{\dots}{ other arguments, usually ignored }
  \item{y}{ optional time for constructing miDateTime from miDate (defaults to 
  	midnight}
  \item{format}{ character, as per \code{\link{strftime}} }
  \item{mark}{boolean: mark times with dangling seconds using '+'}
  }
\details{
  Creating a temporal object with these methods ultimately calls one of the 
  dot.numeric() functions, each of which round their first argument to zero
  places.  This means that all comparisons are based on whole numbers, and 
  therefore not subject to rounding errors.  
  
  Seconds that are not multiples of 60 can be stored in miTime and miDateTime 
  objects, but will not be displayed by default (see above).  miDate can only 
  store numbers of seconds that correspond to midnight, so adding an miDate and 
  miTime gives a predictable result.  
  
  The miTemporal classes are all subclasses of 'numeric', so numeric methods are
  available (but may not preserve class information; try max() for example).
  
  miTemporal classes support NA, Inf, -Inf, as.data.frame, seq, subset, element
  selection, element assignment, and interconversion.
}
\value{
  \item{format}{character}
  \item{as.miTime}{object with class c("miTime","miTemporal","numeric")}
  \item{as.miDate}{object with class c("miDate","miTemporal","numeric")}
  \item{as.miDateTime}{object with class c("miDateTime","miTemporal","numeric")}
}
\references{ http://mifuns.googlecode.com }
\author{ Tim Bergsma }
\seealso{ 
	\code{\link{c.miTemporal}} 
}
\examples{
#numeric to temporal
as.miTime(0)
# 00:00
as.miTime(1)
# 00:00+
as.miTime(-1)
# 23:59+
as.miTime(60)
# 00:01
as.miTime(-60)
# 23:59
as.miTime(86400)
# 00:00
as.miTime(86460)
# 00:01
as.miDate(0)
# 01/01/1970
as.miDate(1)
# 01/01/1970
as.miDate(-1)
# 12/31/1969
as.miDate(-86400)
# 12/31/1969
as.miDate(-86401)
# 12/30/1969
as.miDateTime(0)
# 01/01/1970 00:00
as.miDateTime(60)
# 01/01/1970 00:01 
as.miDateTime(61)
# 01/01/1970 00:01+
as.miDateTime(-1)
# 12/31/1969 23:59+

#character to temporal
as.miTime("00:00")
# 00:00
as.miTime("23:59")
# 23:59
as.miTime("23:59:00")
# 23:59
as.miTime("23:59:01")
# 23:59
as.miTime("23:59:01",format="\%H:\%M:\%S")
# 23:59+
as.miTime("24:00")
# NA
as.miDate("1970-01-02")
# 01/02/1970
as.miDate("01/02/1970",format="\%m/\%d/\%Y")
# 01/02/1970
as.miDateTime("01/02/1970 12:30",format="\%m/\%d/\%Y \%H:\%M")
# 01/02/1970 12:30
as.miDateTime("01/02/1970 12:30:15",format="\%m/\%d/\%Y \%H:\%M:\%S")
# 01/02/1970 12:30+

#temporal to numeric
as.numeric(as.miTime(0))
# 0
as.numeric(as.miTime(1))
# 1
as.numeric(as.miTime(-1))
# 86399
as.numeric(as.miTime(60))
# 60
as.numeric(as.miTime(-60))
# 86340
as.numeric(as.miTime(86400))
# 0
as.numeric(as.miTime(86460))
# 60
as.numeric(as.miDate(0))
# 0
as.numeric(as.miDate(1))
# 0
as.numeric(as.miDate(-1))
# -86400
as.numeric(as.miDate(-86400))
# -86400
as.numeric(as.miDate(-86401))
# -172800
as.numeric(as.miDateTime(0))
# 0
as.numeric(as.miDateTime(60))
# 60 
as.numeric(as.miDateTime(61))
# 61
as.numeric(as.miDateTime(-1))
# -1
as.numeric(as.miTime("00:00"))
# 0
as.numeric(as.miTime("23:59"))
# 86340
as.numeric(as.miTime("23:59:00"))
# 86340
as.numeric(as.miTime("23:59:01"))
# 86340
as.numeric(as.miTime("23:59:01",format="\%H:\%M:\%S"))
# 86341
as.numeric(as.miTime("24:00"))
# NA
as.numeric(as.miDate("1970-01-02"))
# 86400
as.numeric(as.miDate("01/02/1970",format="\%m/\%d/\%Y"))
# 86400
as.numeric(as.miDateTime("01/02/1970 12:30",format="\%m/\%d/\%Y \%H:\%M"))
# 131400
as.numeric(as.miDateTime("01/02/1970 12:30:15",format="\%m/\%d/\%Y \%H:\%M:\%S"))
# 131415

#temporal to character
as.character(as.miTime(0))
# "00:00"
as.character(as.miDate(0))
# "01/01/1970"
as.character(as.miDateTime(0))
# "01/01/1970 00:00"

#non-default printout
format(as.miTime(30000),format="\%H")
# "08"
format(as.miDate("1970-01-01"),format="\%d\%b\%y")
# 01Jan70
format(as.miDateTime("1970-01-02 23:30"),format="[\%d/\%m/\%y \%H:\%M:\%S]")
# "[02/01/70 23:30:00]"
format(as.miTime(1))
# "00:00+"
format(as.miTime(1),mark=FALSE)
# "00:00"

#sequence
seq(from=as.miTime("8:00"),to=as.miTime("12:00"))
# 08:00 09:00 10:00 11:00 12:00
seq(from=as.miDate("2008-01-01"),to=as.miDate("2008-01-04"))
# 01/01/2008 01/02/2008 01/03/2008 01/04/2008
seq(from=as.miDateTime("2008-01-01 12:00"),to=as.miDateTime("2008-01-04 12:30"))
# 01/01/2008 12:00 01/02/2008 12:00 01/03/2008 12:00 01/04/2008 12:00

#interconversion
as.miTime(as.miDate("2008-10-14"))
# 00:00
as.miTime(as.miDateTime("2008-10-14 08:00"))
# 08:00
as.miDate(as.miTime("23:59"))
# 01/01/1970
as.miDate(as.miDateTime("2008-10-14 08:00"))
# 10/14/2008
as.miDateTime(as.miTime(6000000))
# 01/01/1970 10:40
as.miDateTime(as.miDate("2008-10-14"))
# 10/14/2008 00:00

#infinity
as.miTime(Inf)
# Inf
as.miDate(Inf)
# Inf
as.miDateTime(Inf)
# Inf
as.miTime(-Inf)
# -Inf
as.miDateTime(Inf) + as.miTime(Inf)
# Inf
as.miDateTime(Inf) + as.miTime(-Inf)
# NA

#comparison
as.miTime("08:00") < as.miTime("09:00")
# TRUE
as.miDate("1970-01-01") > as.miDate("2008-01-01")
# FALSE
as.miDateTime("1970-01-01 08:00") > as.miDate("1970-01-01")
# TRUE

#statistics
max(as.miDate(c("1970-01-01","1980-01-01","1975-01-01")))
# 315532800

#operations
as.miDateTime(0) + as.miTime(60)
# 01/01/1970 00:01
as.miTime(60) + as.miDateTime(0)
# 00:01
}
\keyword{ manip }
