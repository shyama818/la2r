#' Run the regression_com for the evaluation of a subset of columns
#'
#' Run the regression_com function using a subset of the columns of the dataset
#'
#' @param rindex An integer vector of 0 and 1, which shows presence/absence of each column of the training dataset. (1 presence, 0 absence)
#' @param reg_param A double: the regularization parametrer 
#' @param ite_n An integer: the iteration size in the inner loop
#' @param trainlabel A matrix: 1 x n1 matrix of training label where n1 is the number of records.
#' @param traindata A matrix: m x n1 matrix of training data where m is the number of variables and n1 is the number of records. 
#' @param testlabel A matrix: 1 x n2 matrix of test label where n2 is the number of records.
#' @param testdata A matrix: m x n2 matrix of test data where m is the number of variables and n2 is the number of records.
#' @param start An integer vector which shows the original position of the variables. It is returned by the modify_spl function.
#' @param c0type An integer: 1 for classification, 2 for regression
#'
#' @examples
#' \dontrun{
#' f_rbf(c(0, 1, 1, 0, 1), 1e-5, 3, trainlabel, traindata, testlabel, testdata, 10, c(1, 2, 3, 4, 6, 10), 1)
#' }
#'
#' @export
f_rbf <- function(rindex,reg_param,ite_n,trainlabel,traindata,testlabel,testdata,start,c0type){

findex0 <- which(rindex > 0)
findex1 <- c()
for (gk in 1:length(findex0)){
    ri <- findex0[gk]
    s0 <- start[ri]
    s1 <- start[ri+1]-1
    for (gj in s0:s1){
        findex1 <- c(findex1,gj)
    }
}
traindata0 <- traindata[findex1,,drop = FALSE]
testdata0 <- testdata[findex1,,drop = FALSE]

result = regression_com(reg_param, ite_n, trainlabel, traindata0, testlabel, testdata0, c0type);

result$test_error[ite_n] 

}

