\name{prob1}
\alias{prob1}
\alias{ctl1}
\docType{data}
\title{Example data for NONR}
\description{
 prob1 is a data frame for use with the control stream ctl1.  INFN files for use with 
 NONMEM5 or NONMEM6 can be found in the MIfunsExScript directory located in the R libraries 
 directory. After loading the data (see usage), save objects as files, and edit paths as necessary.

 writeLines(ctl1,"1.ctl")
 write.csv(prob1,"prob1.csv",row.names=FALSE)
 
}
\usage{
	data(ctl1)
	data(prob1)
	}
\format{
  prob1 is tabular; the others are rows of text.
}
\keyword{datasets}
