% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_header2.R
\name{table_header2}
\alias{table_header2}
\title{output the headers for the table with the total row and column}
\usage{
table_header2(dp1, dp1C, dp1R, dp1T, fName, colStrings, rowStrings, path)
}
\arguments{
\item{dp1}{dataframe}

\item{dp1C}{dataframe for the column total}

\item{dp1R}{dataframe for the row total}

\item{dp1T}{dataframe for the total}

\item{fName}{A character string for the name of the file}

\item{colStrings}{A character vector eash of which is name of the column variables}

\item{rowStrings}{A character vector eash of which is name of the row variables}

\item{path}{A character string which is the path of the directory in which the headers for the table are output.}
}
\description{
This function output the headers for the table with the total row and column
}
\examples{
\dontrun{
table_header2(dp0M, dp0C, dp0R, dp0T, "All", c("AgeCat", "Treatment"), c("Elig"),
"/home/shinichi/workspace/Repository/___project/test/Tree/testTree/BMI/Treatment/Sex/Country/table/___show")
}

}
