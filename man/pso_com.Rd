% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pso_com.R
\name{pso_com}
\alias{pso_com}
\title{Run the PSO method}
\usage{
pso_com(
  cache_n,
  pop_n,
  gen_n,
  reg_param,
  ite_n,
  trainlabel,
  traindata,
  testlabel,
  testdata,
  odimno,
  start,
  c0type
)
}
\arguments{
\item{cache_n}{An integer: The best N Feature Subsets generated by PSO}

\item{pop_n}{An integer: the population size of the PSO}

\item{gen_n}{An integer: the iteration size of the PSO}

\item{reg_param}{A double: the regularization parametrer}

\item{ite_n}{An integer: the iteration size in the inner loop}

\item{trainlabel}{A matrix: 1 x n1 matrix of training label where n1 is the number of records.}

\item{traindata}{A matrix: m x n1 matrix of training data where m is the number of variables and n1 is the number of records.}

\item{testlabel}{A matrix: 1 x n2 matrix of test label where n2 is the number of records.}

\item{testdata}{A matrix: m x n2 matrix of test data where m is the number of variables and n2 is the number of records.}

\item{odimno}{An integer: the size of the original variables}

\item{start}{An integer vector which shows the original position of the variables. It is returned by the modify_spl function.}

\item{c0type}{An integer: 1 for classification, 2 for regression}
}
\description{
Run the PSO method in
"A novel binary particle swarm optimization"
based on the classification and the regresson methods in
"Scalable Kernel Methods via Doubly Stochastic Gradients".
}
\examples{
\dontrun{
pso_com(20, 10, 5, 1e-5, 1, trainlabel, traindata, testlabel, testdata, 10, c(1, 2, 3, 4, 6, 10), 1)
}

}
