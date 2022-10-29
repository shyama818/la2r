#' Run the classification and the regresson methods
#'
#' Run the classification and the regresson methods in
#' "Scalable Kernel Methods via Doubly Stochastic Gradients".
#'
#' @param reg_param A double: the regularization parametrer 
#' @param ite_n An integer: the iteration number
#' @param trainlabel A matrix: 1 x n1 matrix of training label where n1 is the number of records.
#' @param traindata A matrix: m x n1 matrix of training data where m is the number of variables and n1 is the number of records. 
#' @param testlabel A matrix: 1 x n2 matrix of test label where n2 is the number of records.
#' @param testdata A matrix: m x n2 matrix of test data where m is the number of variables and n2 is the number of records.
#' @param c0type An integer: 1 for classification, 2 for regression
#'
#' @examples
#' \dontrun{
#' regression_com(1e-5,10,trainlabel,traindata,testlabel,testdata,1)
#' }
#'
#' @export
regression_com <- function(reg_param,ite_n,trainlabel,traindata,testlabel,testdata,c0type){

m0 <- nrow(traindata)
ntr <- ncol(traindata)
nte <- ncol(testdata)

mlimit <- 100

# PCA on data. 
if (m0 > mlimit) {
   "--PCA: "
   set.seed(as.integer(Sys.time()))
   v0 <- pca(mlimit, traindata)
   
   traindata = t(v0) %*% traindata
   testdata = t(v0) %*% testdata
} 

m1 <- nrow(traindata)
S0 <- paste("--regularization parameter:", reg_param, sep = " ")
print(S0)
S1 <- paste("--number of variables:", m1, sep = " ")
print(S1)
S2 <- paste("--instances in training data:", ntr, sep = " ")
print(S2)
S3 <- paste("--instances in test data:", nte, sep = " ")
print(S3)

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
k_n <- 1;
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

train_error_mat <- rep(0,ite_n)
test_error_mat <- rep(0,ite_n)

W <- matrix(0, k_n, 2*ite_n*blocksz)

test_preds <- matrix(0, k_n, nte)

batch_idx <- 1:batch_size

step_size0 = 1;
step_size1 = 1e-4;

for (gj in 1:ite_n) {

    P0 <- paste("---iters no:", gj, sep = " ")
    print(P0)
    f_idx <- gj - 1

    batch_idx <- mod(batch_idx + batch_size - 1, ntr) + 1
    batch_data <- traindata[, batch_idx, drop = FALSE]

    set.seed(r*ite_n+f_idx*blocksz)
    ran0 <- sqrt(2*s) * rnorm(blocksz*m1)
    ran1 <- matrix(ran0,blocksz,m1)

    # train batch feature generation
    T1 <- paste("---step 1: random feature generation for training batch ")
    print(T1)
    train_batch_X <- feature(batch_data, ran1, blocksz)

    # test feature generation
    T2 <- paste("---step 2: random feature generation for test data ")
    print(T2)
    testX <- feature(testdata, ran1, blocksz)

    # train prediction
    T3 <- paste("---step 3: prediction for training batch ")
    print(T3)
    train_batch_preds <- matrix(0, k_n, batch_size)
    if (gj > 1){
        for (inner_j in 0:(f_idx-1)){
            inner_s <- (inner_j*2*blocksz+1)
            inner_e <- (inner_j+1)*2*blocksz
            inner_w_idx <- inner_s:inner_e
      
            set.seed(r*ite_n+inner_j*blocksz)
            inner_ran0 <- sqrt(2*s) * rnorm(blocksz*m1)
            inner_ran1 <- matrix(inner_ran0,blocksz,m1)

            train_batch_preds <- train_batch_preds + W[, inner_w_idx, drop = FALSE] %*% feature(batch_data, inner_ran1, blocksz)
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

    train_error_mat[gj] <- train_error
    if (c0type == 1){
        P2 <- paste("---train error:", train_error, sep = " ")
    } else {
        P2 <- paste("---train error (MSE):", train_error, sep = " ")
    }
    print(P2)

    test_error_mat[gj] <- test_error
    if (c0type == 1){
        P3 <- paste("---test error:", test_error, sep = " ")
    } else {
        P3 <- paste("---test error (MSE):", test_error, sep = " ")
    }
    print(P3)


}
if (c0type == 1){
    result <- list(test_error = test_error_mat, test_pred = test_pred_y)
} else {
    result <- list(test_error = test_error_mat, test_pred = test_preds)
}

return(result)

}

