\name{TABR}
\alias{TABR}
\title{ Generate a comma-delimited table file following a successful 
NONMEM run }
\description{
  TABR can be used to generate a comma-delimited table of the output
  from a successful NONMEM run suitable for import into Microsoft Word. 
  TABR can also be used to add 95% CI estimates generated from bootstrap 
  analysis of final NONMEM model. }
\usage{
TABR(ProjectDir, i, concurrent = 1, boot = 0, bootres = NULL, btcol = NULL, btcoll = NULL, btcolh = NULL, outname)
}
\arguments{
  \item{ProjectDir}{System path to the directory containing the NONMEM control (*.ctl) streams.}
  \item{i}{ Control stream number used to generate table file. Should be numeric with no placeholder 
  zeros to left of number, i.e., "1" not "001".
  Example: \code{i=c(1)}}
  \item{concurrent}{Flag to tell TABR how control streams were executed with (1)- or (0)-sequentially(0). 
  Flag should be "0" in Windows.  Default value is "1". Example: \code{concurrent=1}}
  \item{boot}{Flag to describe type of NONMEM run with (0)-normal, (1)-bootstrap, (2)-long runs, 
  and (3)-bootstrap runs across entire cluster. Flag should be "0" on Windows.
  Example: \code{boot=0}}
  \item{bootres}{Complete path plus name of bootstrap results file. Default is NULL. 
   Example: \code{bootres="/Path/to/results/file/filename"}}
  \item{btcol}{ Columns to read from \code{bootres} and import into R. Must be numeric vector of numbers.
  Example: \code{btcol=c(1:4)}}
  \item{btcoll}{ Column header for \code{bootres} column containing lower 95% CI estimate from
  bootstrap analysis. Must be a character vector. Example: \code{btcoll="P2.5.all"}  }
  \item{btcolh}{ Column header for \code{bootres} column containing upper 95% CI estimate from
  bootstrap analysis. Must be a character vector. Example: \code{btcolh="P97.5.all"} }
  \item{outname}{ Name of txt file output by \code{TABR}. Run number and "\_SumTable.txt" will be appended 
  to outname variable to create complete file name for txt file output by \code{TABR}. 
  Must be a character vector. Example: \code{outname="/FullPKRes"}}
}
\references{ MIfuns package available from Metrum Institute website (\code{http://metruminstitute.org/}). }
\author{ Developed by Metrum Institute (Bill Knebel, Tim Bergsma, and others). }
\note{ 
  Specific file format required by \code{TABR} for \code{bootres} if 95% CIs
  are to be added to comma-delimited table file. 
  
  \code{bootres} file should be comma-delimited with first row containing headers for the columns
  and remaining rows containing bootstrap estimates for each parameter estimated in NONMEM.  Columns
  should correspond to parameter name, median/average, lower 95% CI, and upper 95% CI for each 
  parameter estimated in NONMEM. This should be a summary file generated from the bootstrap analysis of the 
  NONMEM control stream specified \code{i}. 

}
\examples{}
\keyword{ manip }
