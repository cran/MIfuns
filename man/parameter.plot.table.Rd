\name{parameter.plot.table}
\alias{parameter.plot.table}
\alias{mcmc.history}
\alias{mcmc.density}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Tabular and graphic summary of MCMC results}
\description{\code{parameter.plot.table} creates history, density and Gelman-Rubin-Brooks plots for
	each parameter in an array of MCMC results and returns a table of summary statistics for the parameters.
	\code{mcmc.history} creates only the history plots, and \code{mcmc.density} creates only the density plots.}
\usage{
	parameter.plot.table(parameter.array)
	mcmc.history(sims.array)
	mcmc.density(sims.array,n=50)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{parameter.array}{3-way array of MCMC simulations with dimensions number of simulations per chain, number of chains, 
		and number of parameters. This is consistent with the \code{parameters.array} element of the list object returned by the \code{bugs}
		function of the \code{R2WinBUGS} package.}
	\item{sims.array}{Output item containing results from WinBUGS fit}
	\item{n}{the number of equally spaced points at which the density is to be estimated.}
}
\details{
	\code{parameter.plot.table} requires functions from the \code{lattice} and \code{coda} packages. See documentation for the 
	\code{coda} package for details on estimation of effective sample size and the time-series standard error, and creation of the
	Gelman-Rubin-Brooks plots. The history, density and Gelman-Rubin-Brooks plots are printed to the current graphics device. 
}
\value{
	A matrix of summary statistics describing the MCMC simulations for each parameter in \code{sims.array}. Each row 
	contains the statistics for one parameter. The columns correspond to the following statistics: mean, 
	standard deviation, naive standard error, time-series standard error, the 2.5th, 25th, 50th, 75th 97.5th percentile,
	and the effective sample size.
}
\references{ MIfuns package available from Metrum Institute website (\code{http://metruminstitute.org/}). }
\author{ Developed by Metrum Institute (Bill Gillespie and others). }
\keyword{hplot}
\keyword{manip}% __ONLY ONE__ keyword per line
