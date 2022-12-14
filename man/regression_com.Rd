% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regression_com.R
\name{regression_com}
\alias{regression_com}
\title{Run the classification and the regresson methods}
\usage{
regression_com(
  reg_param,
  ite_n,
  trainlabel,
  traindata,
  testlabel,
  testdata,
  c0type
)
}
\arguments{
\item{reg_param}{A double: the regularization parametrer}

\item{ite_n}{An integer: the iteration number}

\item{trainlabel}{A matrix: 1 x n1 matrix of training label where n1 is the number of records.}

\item{traindata}{A matrix: m x n1 matrix of training data where m is the number of variables and n1 is the number of records.}

\item{testlabel}{A matrix: 1 x n2 matrix of test label where n2 is the number of records.}

\item{testdata}{A matrix: m x n2 matrix of test data where m is the number of variables and n2 is the number of records.}

\item{c0type}{An integer: 1 for classification, 2 for regression}
}
\description{
Run the classification and the regresson methods in
"Scalable Kernel Methods via Doubly Stochastic Gradients".
}
\examples{
\dontrun{
regression_com(1e-5,10,trainlabel,traindata,testlabel,testdata,1)
}

}
