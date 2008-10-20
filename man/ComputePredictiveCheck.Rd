\name{ComputePredictiveCheck}
\alias{ComputePredictiveCheck}
\title{Computes individual parameters for observed and simulated 
 data}
\description{
 Computes individual patient parameters for 
 observed data and for each simulated study and 
 computes statistics of individual parameters for 
 each study (beta)
}
\usage{
ComputePredictiveCheck(NonmemData, SimulatedData, SimpleFun = c("min",
"max", "mean", "median"),
CombFun = c("Tmin", "Tmax", "AUC"), StatFun = c("min", "max", "mean",
"median", "Q25", "Q75"),
IDname = "ID", TimeName = "TIME", DVname = "DV", CName = "C", Ignore = "C")
}
\arguments{
  \item{NonmemData}{data frame that contains the data set used for simulations. This data 
  frame should contain MDV column. Only records with MDV=0 will be used. }
  \item{SimulatedData}{simulated data (possibly, output of the simulations with uncertainty 
  function)}
  \item{SimpleFun}{list of functions (in quotes) that accept vector of DV values as an input
   and return a scalar. These functions will be used to compute individual parameters.}
  \item{CombFun}{list of functions (in quotes) that accept data frame with three columns, 
  ID, TIME and DV, and return the data frame with two columns, ID and the columns with the name
  that coincide with the name of the function. These functions will be used to compute 
  individual parameters in the case when computation requires not only the vector of observations 
  but also the times of observations. Examples of this type of function are \code{AUC}, \code{Tmin} 
  and \code{Tmax}. The user can also create custom functions and use those with \code{ComputePredictiveCheck}.}
  \item{StatFun}{list of functions (in quotes) that accept vector of values as an input and return
  a scalar. These functions will be used to compute study statistics. Examples of this type of function
  are \code{Q25} and \code{Q75}. The user can also create custom functions and use those with 
  \code{ComputePredictiveCheck}.}
  \item{IDname}{Name of the column that assigns unique patient ID. Default value is \dQuote{ID}. Each of the data 
  frames \code{NonmemData} and code{SimulatedData} should have \code{IDname} column.}
  \item{TimeName}{Name of the column that assigns time of the observation. Default value is \dQuote{TIME}. Each of 
  the data frames \code{NonmemData} and code{SimulatedData} should have \code{TimeName} column.}
  \item{DVname}{Name of the column that assigns dependent variable value. Default value is \dQuote{DV}. Each of the 
  data frames \code{NonmemData} and \code{SimulatedData} should have \code{DVname} column. \code{DVname} 
  column of \code{NonmemData} data frame should contain observed values. \code{DVname} column of 
  \code{SimulatedData} data frame should contain simulated values.}
  \item{CName}{Name of the column that assigns the comment field. Default value is \dQuote{C}. The data frame 
  \code{NonmemData} should have \code{Cname} column. Records with the character assigned by \code{Ignore}
  parameter in the \code{Cname} column will be removed from the analysis.}
  \item{Ignore}{The character that denotes the record of the \code{NonmemData} data frame that should be 
  removed from the analysis. Default value is \dQuote{C}.}
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  The output of the program is the list with two elements, Combined and Stats. Combined is the data frame that
  contains individual parameters. It has the columns ID, STUDY and one column for each element of the 
  \code{SimpleFun} and \code{CombFun} vectors. Stats is the list, a collection of data frames, one per statistics 
  requested. The names of the data frame are given by \code{StatFun} vector. Individual element of the list can be 
  called as \code{Stats[[k]]} where \code{k} is the order of the desired data frame in the \code{StatFun} list. The 
  same element can be called by name as \code{Stats[ StutFun[k] ]}. Each data frame (element of Stats list) contains
  summaries of individual parameters by study. It has STUDY column and one column for each element of the \code{SimpleFun}
  and code{CombFun} vectors. STUDY column give the order number of the simulated study. STUDY=0 record describes 
  observed data.  
}
\references{ MIfuns package available from Metrum Institute website (\code{http://metruminstitute.org/}). }
\author{ Developed by Metrum Institute (Bill Knebel, Tim Bergsma, and others). }
\seealso{ }
\examples{
}
\keyword{ manip }
