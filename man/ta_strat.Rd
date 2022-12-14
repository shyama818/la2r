% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ta_strat.R
\name{ta_strat}
\alias{ta_strat}
\title{A framework to conduct the layered analysis}
\usage{
ta_strat(dsgroup, stratStrings, outputStrings, .fun = NULL, ...)
}
\arguments{
\item{dsgroup}{dataframe}

\item{stratStrings}{A character vector eash of which is name of the layered variables}

\item{outputStrings}{A character vector eash of which is name of the output datasets}

\item{.fun}{A function which is immplemented for the subset for the each realization of the layered variables}

\item{...}{other arguments passed on to .fun}
}
\value{
an R List which contains the output datasets.
}
\description{
This function provides a framework to conduct the layered analysis.
For the subset for each realization of the layered variables,
it immplements a function given by .fun and
output an R List which contains the output datasets.
}
\examples{
\dontrun{
ta_strat(demo, c("Treatment", "Sex"), c("Ethnic"), .fun = function(xx) {
anova1_wrap(xx, "BMI","AgeCat", c("na.omit"))
})
}

}
