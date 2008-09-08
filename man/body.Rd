\name{body}
\alias{bmi}
\alias{bsa}
\alias{ibw}
\alias{lbm}
\alias{crcl}
\title{ Calculate Various Clinical Indicators}
\description{
  Calculate body mass index, body surface area, ideal body weight, 
  lean body mass, or creatinine clearance using common equations.
}
\usage{
bmi(wt, ht)
bsa(wt, ht)
lbm(wt, ht, male)
ibw(ht, male, floor = FALSE)
crcl(age, wt, male, scr)
}
\arguments{
  \item{wt}{ Weight (actually, mass) in kilograms. }
  \item{ht}{ Height in centimeters. }
  \item{age}{ Age in years. }
  \item{male}{A logical indicating sex: TRUE indicates male.}
  \item{floor}{ If TRUE, "inches over 5 feet" is not allowed to be negative. }
  \item{scr}{ serum creatinine in mg/dL. }
}
\details{
	Units of return values are as follows....
	bmi - kg/m\^2
	bsa - m\^2
	lbm - kg 
	ibw - kg
	crcl - mL/min
}
\value{
  Numeric.  
}
\references{
	\describe{
		\item{bmi}{[\url{http://www.halls.md/body-mass-index/bmirefs.htm}]}
		\item{bsa}{(BSA: Gehan & George variation) \url{http://www.halls.md/body-surface-area/refs.htm}}
		\item{lbm}{\url{http://www.halls.md/body-mass-index/leanbody.htm}}
		\item{ibw}{\url{http://www.halls.md/ideal-weight/devine.htm}}
		\item{crcl}{\url{http://en.wikipedia.org/wiki/Renal_function}}
	}
}
\author{ Tim Bergsma}
\examples{
bmi(70,160)
bsa(70,160)
lbm(70,160,TRUE)
ibw(160,TRUE)
crcl(35,70,TRUE,1.0)
}
\keyword{ manip }

