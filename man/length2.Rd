% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/length2.R
\name{length2}
\alias{length2}
\title{Count N}
\usage{
length2(x, na.rm = TRUE)
}
\arguments{
\item{x}{dataframe}

\item{na.rm}{a character string (TRUE or FALSE) for the parameter na.rm}
}
\value{
N
}
\description{
New version of the length function which can handle NA's: if na.rm==T, it doesn't count them
}
\examples{
\dontrun{
length2(demo, na.rm=TRUE)
}d

}
