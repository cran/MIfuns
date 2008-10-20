\name{MIfuns-package}
\alias{MIfuns-package}
\alias{MIfuns}
\docType{package}
\title{
Pharmacometric tools for data preparation, analysis, simulation, and reporting
}
\description{
Pharmacometric tools for common data preparation tasks, stratified
bootstrap resampling of data sets, NONMEM control stream
creation/editing, NONMEM model execution, creation of standard and
user-defined diagnostic plots,  execution and summary of bootstrap and
predictive check results, implementation of simulations from posterior
parameter distributions, reporting of output tables and creation of a
detailed analysis log. All functions are considered stable unless marked
as beta.
}
\details{
  \tabular{ll}{
    \code{\link{CLNR}} \tab Delete files and directories; log what was
    deleted to a text file.\cr
    \code{\link{compute.cwres}} \tab Compute the conditional weighted
    residuals (ind.cwres,read.cwres.data,is.cwres.readable).\cr
    \code{\link{CWRES}} \tab Generate conditional weighted residual
    (CWRES) diagnostic plots as a pdf (cwres\_1).\cr
    \code{\link{NONR}} \tab Run NONMEM and create diagnostic plots.\cr
    \code{\link{PLOTR}} \tab Generate diagnostic and/or covariate
    evaluation plots as a pdf or multiple pngs.\cr
    \code{\link{rlog}} \tab Generate a combined run log from multiple
    NONMEM runs.\cr
    \code{\link{resample.data.frame}} \tab Create replicate datasets by
    stratified sampling with replacement
    (as.csv.filename,resample.csv.filename).\cr
    \code{\link{metaSub.character}} \tab Make multiple, systematic
    substitutions in text files (as.filename,metaSub.filename).\cr
    \code{\link{CreateParametersForSimulation}} \tab Create parameters
    for simulation from uncertainity.\cr
    \code{\link{as.miDate}} \tab Create date object with default
    behaviors appropriate for NMTRAN datasets.\cr
    \code{\link{as.miTime}} \tab Create time object with default
    behaviors appropriate for NMTRAN datasets.\cr
    \code{\link{as.miDateTime}} \tab Create date-time object with
    default behaviors appropriate for NMTRAN datasets.\cr
    \code{\link{bsa}} \tab Calculate various clinical measurements: body
    size, etc. (bsa, bmi, ibw, lbm, crcl).\cr
    \code{\link{constant}} \tab Test for nesting among factors(nests,
    nested.in, crosses).\cr
    \code{\link{naInContext}} \tab View missing values in a broader
    context.\cr
    \code{\link{stableMerge}} \tab Add keyed data to a dataframe
    safely.\cr
    \code{\link{css}} \tab Calculate One-Compartment Model Properties
    (acr, cavg, cmax, cmin, ke, tmax, auc).\cr
    \code{\link{bwplot.distribution}} \tab Make "forest plots" of
    covariate effects, etc.\cr
    \code{\link{forbak}} \tab Impute missing vector values(locf,
    nocb).\cr
    \code{\link{runhead}} \tab Check some properties of vectors and data
    frames(maxChar, prev, sorted).\cr
    \code{\link{bin}} \tab Calculate bin sizes and limits for a
    vector.\cr
    \code{\link{accept}} \tab Document acceptance of an R installation
    (acceptance, packageCheck).
    
  }
    
    \tabular{ll}{
Package: \tab MIfuns\cr
Type: \tab Package\cr
Version: \tab 3.2.7\cr
Date: \tab 2008-10-20\cr
License: \tab GPL\cr
}

  }
  
  \author{
    Developed by Metrum Institute (Bill Knebel, Tim Bergsma, and
    others). Maintainer: Bill Knebel <billk@metrumrg.com>
  }
  
  \references{
    MIfuns package available from Metrum Institute website (\code{http://metruminstitute.org/}). }
  \keyword{
    manip }
