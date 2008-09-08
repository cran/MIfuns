\name{myshell}
\alias{myshell}
\title{Run a DOS command without opening a command window}
\description{
	\code{myshell} runs a DOS command without opening a command window. \code{myshell(cmd)} is
	just a synonym for
	\quote{\code{system(paste(Sys.getenv("COMSPEC"),"/c",cmd), intern = FALSE, wait = TRUE, input = NULL,
		show.output.on.console = FALSE, minimized = FALSE, invisible = TRUE)}}
}
\usage{
	myshell(cmd)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cmd}{DOS command as a character string}
}
\value{
	See documentation for \code{system}.
}
\references{ MIfuns package available from Metrum Institute website (\code{http://metruminstitute.org/}). }
\author{ Developed by Metrum Institute (Bill Gillespie and others). }
\keyword{utilities}% __ONLY ONE__ keyword per line
