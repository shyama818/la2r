#' Run the MKL method (inner loop)
#'
#' Run the inner loop of the MKL method in 
#' "Simple and Efficient Multiple Kernel Learning by Group Lasso"
#' based on the classification and the regresson methods in
#' "Scalable Kernel Methods via Doubly Stochastic Gradients".
#'
#' @param gk An integer: the current iteration number in the outer loop
#' @param gen_n An integer: the iteration size in the outer loop
#' @param theta A numeric vector: the weight of the kernel
#' @param p0 A double: The p-norm which is a costraint on the coefficients of the kernels
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
#' mregression_com(2, 3, c(0.1667, 0.1667, 0.1667, 0.1667, 0.1667, 0.1667), 2, 1e-5, 3, trainlabel, traindata, testlabel, testdata, 10, c(1, 2, 3, 4, 6, 10), 1)
#' }
#'
#' @export
mregression_com <- function(gk, gen_n, theta, p0, reg_param, ite_n, trainlabel, traindata, testlabel, testdata, odimno, start, c0type){

m2a <- odimno+1
m1 <- nrow(traindata)
ntr <- ncol(traindata)
nte <- ncol(testdata)

S0 <- paste("--regularization parameter:", reg_param, sep = " ")
print(S0)
S1 <- paste("--number of variables:", m1, sep = " ")
print(S1)
S2 <- paste("--instances in training data:", ntr, sep = " ")
print(S2)
S3 <- paste("--instances in test data:", nte, sep = " ")
print(S3)

S4 <- paste("--number of kernels:", m2a, sep = " ")
print(S4)
S5 <- paste("--kernel weights:", theta, sep = " ")
print(S5)

if (c0type == 1){

uniq <- unique(as.vector(trainlabel))
k_n <- length(uniq)

# Assuming label = {1,...,m1}
trainY <- matrix(0,k_n, ntr)
for (i in 1:ntr){
	trainY[trainlabel[i],i] <- 1
}
testY <- matrix(0,k_n, nte)
for (i in 1:nte){
	testY[testlabel[i],i] <- 1
}

} else {
k_n <- 1
trainY <- matrix(0,k_n, ntr)
testY <- matrix(0,k_n, nte)
}

set.seed(as.integer(Sys.time()))
s <- mtrick(traindata)

# todo
blocksz <- 2^11

en <- 15
batch_size <- min(ntr,2^en)

# Random seed offset
r <- 0

train_error_mat <- matrix(0,1,ite_n)
test_error_mat <- matrix(0,1,ite_n)

W <- matrix(0, k_n, 2*ite_n*blocksz)

test_preds <- matrix(0, k_n, nte)

batch_idx <- 1:batch_size

step_size0 = 1;
step_size1 = 1e-4;

amin <- min(ntr, 2^en)
set.seed(as.integer(Sys.time()))
allI <- sample.int(ntr)
trainAll <- traindata[,allI[1:amin],drop = FALSE]

train_all_preds <- array(c(0), dim = c(k_n, amin, m2a))
for (gj in 1:ite_n) {

    P10 <- paste("--outer loop:", gk, "--inner loop:", gj, sep = " ")
    print(P10)
    f_idx <- gj - 1

    batch_idx <- mod(batch_idx + batch_size - 1, ntr) + 1
    batch_data <- traindata[, batch_idx, drop = FALSE]

    set.seed(r*ite_n+f_idx*blocksz)
    ran0 <- sqrt(2*s) * rnorm(blocksz*m1)
    ran1 <- matrix(ran0,blocksz,m1)
    
    # train batch feature generation
    T1 <- paste("---step 1: random feature generation for training batch ")
    print(T1)
    train_batch_X <- featurem(batch_data, theta, ran1, blocksz, odimno, start)
    
    # test feature generation
    T2 <- paste("---step 2: random feature generation for test data ")
    print(T2)
    testX <- featurem(testdata, theta, ran1, blocksz, odimno, start)
    
    # train prediction
    T3 <- paste("---step 3: prediction for training batch ")
    print(T3)
    train_batch_preds <- matrix(0, k_n, batch_size)
    if (gj > 1){
        for (inner_j in 0:(f_idx-1)){
            set.seed(r*ite_n+inner_j*blocksz)
            inner_ran0 <- sqrt(2*s) * rnorm(blocksz*m1)
            inner_ran1 <- matrix(inner_ran0,blocksz,m1)

            train_batch_preds <- train_batch_preds + W[, (inner_j*2*blocksz+1):((inner_j+1)*2*blocksz)] %*% featurem(batch_data, theta, inner_ran1, blocksz, odimno, start)
        }
    }
    if (c0type == 1){
        residue <- softmax(train_batch_preds) - trainY[, batch_idx, drop = FALSE]
    }else{
        residue <- train_batch_preds - trainlabel[, batch_idx, drop = FALSE]
    }

    step_size <- step_size0 / (1 + step_size1 * gj)
    ind_s <- (f_idx*2*blocksz+1)
    ind_e <- (f_idx+1)*2*blocksz
    w_idx <- ind_s:ind_e
    updateW <- update_w(step_size, reg_param, batch_size, blocksz, residue, W[, w_idx, drop = FALSE], train_batch_X)

    W[, w_idx] <- W[, w_idx, drop = FALSE] + updateW
    if (reg_param > 1e-6 && gj > 1){
        for (inner_j in 0:(f_idx-1)){
            inner_s <- (inner_j*2*blocksz+1)
            inner_e <- (inner_j+1)*2*blocksz
            inner_w_idx <- inner_s:inner_e
            W[, inner_w_idx] <- (1 - step_size * reg_param) * W[, inner_w_idx, drop = FALSE]
        }
    }

    # evaluation
    T4 <- paste("---step 4: evaluation ")
    print(T4)
    if (c0type == 1){
        train_preds_batch <- train_batch_preds + updateW %*% train_batch_X
        train_pred_y <- t(max.col(t(train_preds_batch)))
        dif <- train_pred_y - trainlabel[,batch_idx,drop = FALSE]
        train_error <- 1 - sum(isZero(dif)) / batch_size

        test_preds <- test_preds + updateW %*% testX
        test_pred_y <- t(max.col(t(test_preds)))
        dif2 <- test_pred_y - testlabel
        test_error <- 1 - sum(isZero(dif2)) / nte

    } else {
        train_preds_batch <- train_batch_preds + updateW %*% train_batch_X
        dif <- train_preds_batch - trainlabel[,batch_idx,drop = FALSE]
        normdif <- norm(dif, type="2")
        train_error <- (normdif^2) / batch_size

        test_preds <- test_preds + updateW %*% testX
        dif2 <- test_preds - testlabel
        normdif2 <- norm(dif2, type="2")
        test_error <- (normdif2^2) / nte
    }

    train_error_mat[1,gj] <- train_error
    if (c0type == 1){
        P2 <- paste("---train error:", train_error, sep = " ")
    } else {
        P2 <- paste("---train error (MSE):", train_error, sep = " ")
    }
    print(P2)

    test_error_mat[1,gj] <- test_error
    if (c0type == 1){
        P3 <- paste("---test error:", test_error, sep = " ")
    } else {
        P3 <- paste("---test error (MSE):", test_error, sep = " ")
    }
    print(P3)

    if (gj == ite_n & gk != gen_n){
        T5 <- paste("---step 5: update of the kernel weights ")
        print(T5)
        sum_denom <- 0
        theta0 <- rep(0,m2a)
        for (ri in 1:odimno){
            ric <- ri-1
            train_all_X <- feature2(trainAll, ran1, blocksz, start, ric)
            train_all_pred0 <- matrix(train_all_preds[,,ri], k_n, amin)
            train_all_pred1 <- train_all_pred0 + updateW %*% train_all_X
            train_all_preds[,,ri] <- train_all_pred1

            norm0 <- train_all_pred1 * train_all_pred1
            norm1 <- mean(colSums(norm0, na.rm = TRUE), na.rm = TRUE)
            norm2 <- norm1^(p0/(1+p0))
            sum_denom <- sum_denom + norm2

            theta0[ri] <- norm1^(1/(1+p0))
        }
        train_all_X <- feature(trainAll, ran1, blocksz)
        train_all_pred0 <- matrix(train_all_preds[,,m2a], k_n, amin)
        train_all_pred1 <- train_all_pred0 + updateW %*% train_all_X
        train_all_preds[,,m2a] <- train_all_pred1

        norm0 <- train_all_pred1 * train_all_pred1
        norm1 <- mean(colSums(norm0, na.rm = TRUE), na.rm = TRUE)
        norm2 <- norm1^(p0/(1+p0))
        sum_denom <- sum_denom + norm2

        theta0[m2a] <- norm1^(1/(1+p0))

        norm3 <- sum_denom^(1/p0)
        theta <- theta0/norm3

    }

}
if (c0type == 1){
    result <- list(test_error = test_error_mat, test_pred = test_pred_y, theta = theta)
} else {
    result <- list(test_error = test_error_mat, test_pred = test_preds, theta = theta)
}
return(result)

}

