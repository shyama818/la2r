% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wilp2_wrap.R
\name{wilp2_wrap}
\alias{wilp2_wrap}
\title{Conduct Wilcoxon Signed Rank test for ordered factor variable}
\usage{
wilp2_wrap(x0, lhs, rhs, paracode)
}
\arguments{
\item{x0}{dataframe}

\item{lhs}{A character string which is name of the main variable of ordered factor type}

\item{rhs}{A character string which is name of the group variable of factor type with two levels}

\item{paracode}{A character vector which is used for the specification of the 4 parameters (alternative, correct, conf.level, na.action).}
}
\value{
an R List which contains one dataframe;
wil_signedrank_result.
}
\description{
This function immplements wilcox.test with paired = TRUE after checking that each group in the input dataset has at least 2 non-null records
}
\examples{
\dontrun{
wilp2_wrap(demo, "BMI", "Sex", c("two.sided", "TRUE", "0.95", "na.omit"))
}

}
