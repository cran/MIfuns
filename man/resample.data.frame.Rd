\name{resample.data.frame} 
\alias{resample}
\alias{as.csv.filename}
\alias{as.csv.filename.character}
\alias{resample.csv.filename}
\alias{resample.data.frame}
\title{Create Replicate Data Sets by Stratified Sampling With Replacement}
\description{
	\code{resample()} is generic.  A Methods is defined for data.frame; 
	a convienience wrapper is provided for passing names of csv files 
	to be read and then resampled.
}
\usage{
	\method{as.csv.filename}{character}(x, ...)
	\method{resample}{csv.filename}(x, ...)
	\method{resample}{data.frame}(x, names, key = NULL, rekey = FALSE, out = NULL, stratify = NULL, seed = 0, ...)
}

\arguments{
  \item{x}{A data.frame, or (second form) a file name for a CSV file to read.}
  \item{names}{A list of names for replicate data sets; can be a simple vector.}
  \item{key}{An atomic character value naming the column in x that distinguishes
  unique individuals, (resampling targets). Defaults to row names.}
  \item{rekey}{If true, key values in resampled data sets will have unique values 
  of key replaced with consecutive integers, starting at 1.}
  \item{out}{A (path and) directory in which to write resulting data sets as CSV files.}
  \item{stratify}{A list of factors, the interactions of which will be the levels of 
  stratification.  Each factor must have the same length as nrow(x).  Or a character vector
  of names in names(x).}
  \item{seed}{A number to use as the seed for the random number generator.}
  \item{\dots}{Extra arguments, currently ignored.}
}
\details{
	Typical usages are
	\preformatted{
		resample(x, names, ...)
		resample(as.csv.filename(x), names, ...)
	}
	The argument \code{key} gives the name of the column in x to identify
	unique experimental units (individuals). If not supplied, a temporary
	key is constructed from the row names, and sampling occurs at the row level.
	
	The number of resamplings is controlled by the length of \code{names}. 
	\code{names} is coerced to character, and each value is used to name 
	a *.csv file, if \code{out} is supplied. If \code{out} is omitted, a list
	of data.frame is returned.
	
	\code{stratify} is a list of factors, or items that can be coerced to 
	factors.  Currently \code{stratify} is coerced to a data.frame for
	convenient manipulation.  Empty levels are dropped. If \code{stratify} is 
	not supplied, the whole data set is treated as a single level. Otherwise,
	each resulting data set has as many keys in each level as the original.
	An error results if \code{key} is not nested within \code{stratify}.
}
\value{
	A list of data.frame, or if \code{out} is supplied, an invisible list of
	the numbers of rows of each data.frame written to file.
}
\note{ resample.character, etc. has been moved to metaSub.character, etc.}
\references{This package developed under the auspices of Metrum Institute,
	http://metruminstitute.org}
\author{Tim Bergsma}

\seealso{\code{\link{metaSub.character}}}
\examples{
b <- resample(Theoph,key="Subject",names=1:3)
d <- resample(
	Theoph,
	key="Subject",
	rekey=TRUE,
	names=1:3,
	out=".",
	stratify=Theoph$Dose < mean(Theoph$Dose)
)
e <- resample(as.csv.filename("1.csv"),names="theoph")
}
\keyword{ manip }
