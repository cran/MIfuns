\name{PlotPredictiveCheck}
\alias{PlotPredictiveCheck}
\title{Plots results of the ComputePredictiveCheck function}
\description{
 The function \code{PlotPredictiveCheck} uses the output list of the 
 \code{ComputePredictiveCheck} function to create histograms that compare
 distribution of study statistics across the simulated studies with 
 the statistic computed for the observed data.
}
\usage{
PlotPredictiveCheck(PCData, Parameters = c("min", "max", "mean", "median", "Tmin", "Tmax", "AUC"), Summaries = c("min", "max", "mean", "median", "Q25", "Q75"), plot.hist = TRUE, plot.qq = TRUE, DrugName = "DV", plotType = "pdf", FileTemplate = "Plots")
}
\arguments{
  \item{PCData}{The output of the \code{ComputePredictiveCheck} function}
  \item{Parameters}{list of individual parameter names (in quotes) that should be
   illustrated by plots. These parameter names should be either from \code{SimpleFun}
   or \code{CombFun} lists of the \code{ComputePredictiveCheck} function call 
   that created \code{PCData}.  }
  \item{Summaries}{list of summary statistics that are used to characterize the distribution
   of individual parameters.  These parameter names should be from the \code{StatFun} list of 
   the \code{ComputePredictiveCheck} function call that created \code{PCData}.}
  \item{plot.hist}{equal to TRUE (default value) if histogram plot is requested and FALSE otherwise.}
  \item{plot.qq}{equal to TRUE (default value) if QQ-plot is requested and FALSE otherwise.}
  \item{DrugName}{Name of the drug (default value is \dQuote{DV}) that will be used in the  plots. Limited control
   over plot labels and title exist. The function needs to be modified in order to create more precise 
   titles and labels.}
  \item{plotType}{Character name (in quotes) that assigns the device for the output plots. Supported 
  names are \dQuote{wmf}, \dQuote{pdf}, \dQuote{ps}, \dQuote{png}, \dQuote{fpeg}, and \dQuote{bmp} that initiate the appropriate R graphic 
  device and create one or several output files. The default value \dQuote{pdf} is suitable for the exploratory 
  investigation of the data. For report-quality plots, on MSWindows only, \dQuote{wmf} is recommended.  If \code{PlotType} is \dQuote{pdf} 
  or \dQuote{ps}, one file with all the figures is created. For any other \code{PlotType}, one file per plot is 
  created. Extension of the file coincides with the \code{PlotType}.}
  \item{FileTemplate}{File name template that will be used to create the output file name. If one file 
  with all the plots is created (\code{PlotType} is \dQuote{pdf} or \dQuote{ps}) then the \code{PlotType} extension will 
  be added to the \code{FileTemplate} name to create the name of the output file. If one file per plot is created 
  (\code{PlotType} is not equal to \dQuote{pdf} or \dQuote{ps}) then the three-digit order number will be added to the 
  \code{FileTemplate} name. Then the \code{PlotType} extension will be added to create the name of the output file.}
}
\details{
}
\value{
   The output of the program is one or more files with the plots. If \code{plot.hist} is TRUE, then for each 
   combination of the elements from the \code{Parameters} and \code{Statistics} lists, the histogram of simulated 
   values is created; observed value is shown by the vertical line; probability of observing statistics as extreme 
   as observed data is presented in the title of each histogram. If \code{plot.qq} is TRUE, then for each element 
   of the \code{Parameters} lists, the quantile-quantile plots are created. The plots superimpose quantile-quantile 
   curves of the parameter distributions in each of the simulated studies against the observed parameter distribution. 
   The unit line is provided for the reference.}
\references{ MIfuns package available from Metrum Institute website (\code{http://metruminstitute.org/}). }
\author{ Developed by Metrum Institute (Bill Knebel, Tim Bergsma, and others). }
\seealso{ }
\examples{
}
\keyword{ manip }
