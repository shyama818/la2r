#' Run the MKL method (outer loop)
#'
#' Run the outer loop of the MKL method in 
#' "Simple and Efficient Multiple Kernel Learning by Group Lasso"
#' based on the classification and the regresson methods in
#' "Scalable Kernel Methods via Doubly Stochastic Gradients".
#'
#' @param gen_n An integer: the iteration size in the outer loop
#' @param pnorm A double: The p-norm which is a costraint on the coefficients of the kernels
#' @param reg_param A double: the regularization parametrer 
#' @param ite_n An integer: the iteration size in the inner loop
#' @param trainlabel A matrix: 1 x n1 matrix of training label where n1 is the number of records.
#' @param traindata A matrix: m x n1 matrix of training data where m is the number of variables and n1 is the number of records. 
#' @param testlabel A matrix: 1 x n2 matrix of test label where n2 is the number of records.
#' @param testdata A matrix: m x n2 matrix of test data where m is the number of variables and n2 is the number of records.
#' @param odimno An integer: the size of the original variables 
#' @param start An integer vector which shows the original position of the variables. It is returned by the modify_spl function.
#' @param c0type An integer: 1 for classification, 2 for regression
#'
#' @examples
#' \dontrun{
#' mkl_com(3, 2, 1e-5, 3, trainlabel, traindata, testlabel, testdata, 10, c(1, 2, 3, 4, 6, 10), 1)
#' }
#'
#' @export
mkl_com <- function(gen_n, pnorm, reg_param, ite_n, trainlabel, traindata, testlabel, testdata, odimno, start, c0type){

m2a <- odimno+1

sqr0 <- m2a^(1.0/pnorm)
sqr1 <- 1.0/sqr0
theta <- rep(sqr1,m2a)
testError <- matrix(0,gen_n,ite_n)

for (gk in 1:gen_n) {
    result <- mregression_com(gk, gen_n, theta, pnorm, reg_param, ite_n, trainlabel, traindata, testlabel, testdata, odimno, start, c0type)
    result2 <- list(test_error = result$test_error, test_pred = result$test_pred, theta = theta)
    theta <- result$theta
    testError[gk,] <- result$test_error
}

print(testError)
return(result2)

}

