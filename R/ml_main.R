#' Implement classification and regresson
#'
#' Implement classification and regresson with SVM, MKL and PSO  
#'
#' @param dsgroup dataframe: input dataframe
#' @param xNames A string vector: the names of the input variables
#' @param yName A string: the name of the target variable
#' @param vec An integer vector (size of xNames): type of input variables (1 numerical variable, 2 ordered factor variable, 3 un-ordered factor variable)
#' @param svmType An integer: 1 classification (SVM), 2 regression (SVM), 3 classification (MKL), 4 regression (MKL), 5 classification (PSO), 6 regression (PSO),  
#' @param tSize An integer vector: (the percent for the training dataset, the percent for the test dataset, the percent for the validation dataset)
#' @param reg_param0 A double vector: the regularization parametrer vector; if size > 1, the best parameter is searched using the validation dataset.  
#' @param pSize An integer vector: (Population of PSO, Iteration number (PSO, MKL), Iteration number (SVM), p-norm of MKL, The best N Feature Subsets (PSO)) 
#'
#' @return dataframe of the input variables and the target variable with no missing values  
#'
#' @examples
#' \dontrun{
#' ml_main(demo,c("Bssys","Bsdia","Blwt","Blht","AgeCat","Wt2Cls","Sex","treatment","Elig","Crclct"), c("BMI"), c(1,1,1,1,2,2,3,3,3,3), 2, 
#' c(70, 30, 0), c("1e-5"), c(NA, NA, 7, NA, NA))
#' }
#'
#' @export
ml_main <- function(dsgroup, xNames, yName, vec, svmType, tSize, reg_param0, pSize){

x00 <- dsgroup[, xNames, drop = FALSE]
y00 <- dsgroup[, yName, drop = FALSE]

spl_result <- spl_factor(x00, vec)
x01 <- spl_result$data
start0 <- spl_result$start0
start1 <- spl_result$start1
start2 <- spl_result$start2

x10 <- data.matrix(x01)
y10 <- data.matrix(y00)

        n1 <- nrow(x10)
        m1 <- ncol(x10)

        # change the range of x to (-1,1)

        tmaxx <- apply(x10, 2, max, na.rm = TRUE)
        tminx <- apply(x10, 2, min, na.rm = TRUE)
        tdifx <- tmaxx-tminx

        mminx <- repmat(tminx,n1,1)
        mdifx <- repmat(tdifx,n1,1)

        x1 <- x10-mminx
        x2 <- 2*x1
        x3 <- x2/mdifx;
        x11 <- x3-1

        # change the range of y to (-1,1)

        if (svmType == 2 | svmType == 4 | svmType == 6) {

            y0 <- as.vector(y10)
            tmaxy <- max(y0, na.rm = TRUE)
            tminy <- min(y0, na.rm = TRUE)
            tdify <- tmaxy-tminy

            y1 <- y0-tminy
            y2 <- 2*y1
            y3 <- y2/tdify
            y11 <- y3-1

            #y003 <- y11+1
            #y002 <- y003*tdify
            #y001 <- y002/2
            #y00ck <- y001+tminy
            #y00check <- cbind(y10,y00ck)

        } else {
            # Transform to {1,...,m}
            y0 <- as.vector(y10)
            y1 <- unique(y0)
            y2 <- rank(y1)
            if (all(y1 == y2)) {
                y11 <- y0
            } else {
                y11 <- mapvec(y0, y1, y2)
            }
        }
        
        x12 <- t(x11)
        y12 <- matrix(y11,1,n1)

# Training & Test

trainSize <- tSize[1]
testSize <- tSize[2]
varSize <- tSize[3]

train_size <- floor(n1*trainSize/100)
if (varSize > 0){
     test_size <- floor(n1*testSize/100)
     var_size <- n1 - train_size - test_size
} else {
     test_size <- n1 - train_size
     var_size <- 0
}

set.seed(as.integer(Sys.time()))
rperm = sample.int(n1)

traindata0 <- x12[,rperm[1:train_size],drop = FALSE]
testdata0 <- x12[,rperm[(train_size+1):(train_size+test_size)],drop = FALSE]
if (varSize > 0){
    vardata0 <- x12[,rperm[(train_size+test_size+1):(train_size+test_size+var_size)],drop = FALSE]
} else {
    vardata0 <- NULL
}

trainlabel <- y12[,rperm[1:train_size],drop = FALSE]
testlabel <- y12[,rperm[(train_size+1):(train_size+test_size)],drop = FALSE]
if (varSize > 0){
    varlabel <- y12[,rperm[(train_size+test_size+1):(train_size+test_size+var_size)],drop = FALSE]
} else {
    varlabel <- NULL
}

modify_result <- modify_spl(traindata0, testdata0, vardata0, start0, start1, start2)
traindata <- modify_result$train
testdata <- modify_result$test
vardata <- modify_result$var
olabel <- modify_result$olabel
odimno <- modify_result$odimno
start <- modify_result$start

# Hyper-parameter

if (length(reg_param0)>1){
    vErr <- rep(0,length(reg_param0))
    iten0 <- ite_n
    if (svmType == 1 || svmType == 3 || svmType == 5){
        c0type <- 1
    } else {
        c0type <- 2
    }
    for (gi in 1:length(reg_param0)){
        result <- regression_com(as.double(reg_param0[gi]),iten0,trainlabel,traindata,varlabel,vardata,c0type)
        vErr[gi] <- result$test_error[iten0]
    }
    mind <- which.min(vErr)
    reg_param <- as.double(reg_param0[mind]) 
} else {
    reg_param <- as.double(reg_param0[1])
}


# Main Computation

pop_n <- pSize[1]
gen_n <- pSize[2]
ite_n <- pSize[3]
pnorm <- pSize[4]
cache_n <- pSize[5]

if (svmType == 1){
    result <- regression_com(reg_param, ite_n, trainlabel, traindata, testlabel, testdata, 1) 
} else if (svmType == 2){
    result <- regression_com(reg_param, ite_n, trainlabel, traindata, testlabel, testdata, 2) 
} else if (svmType == 3){
    result <- mkl_com(gen_n, pnorm, reg_param, ite_n, trainlabel, traindata, testlabel, testdata, odimno, start, 1)  
} else if (svmType == 4){
    result <- mkl_com(gen_n, pnorm, reg_param, ite_n, trainlabel, traindata, testlabel, testdata, odimno, start, 2) 
} else if (svmType == 5){
    result <- pso_com(cache_n, pop_n, gen_n, reg_param, ite_n, trainlabel, traindata, testlabel, testdata, odimno, start, 1)  
} else if (svmType == 6){
    result <- pso_com(cache_n, pop_n, gen_n, reg_param, ite_n, trainlabel, traindata, testlabel, testdata, odimno, start, 2) 
}

if (svmType == 5 | svmType == 6){
   best_results <- data.frame(best_results = result$best_results)
   if (svmType == 5) {
        colnames(best_results) <- c("test error")
   } else if (svmType == 6) {
        colnames(best_results) <- c("test error (MSE)")
   }
   best_subsets0 <- data.frame(result$best_subsets)
   colnames(best_subsets0) <- olabel
   best_subsets <- cbind(best_results, best_subsets0)
} else {
   # prediction
   test_pred0 <- result$test_pred
   test_pred1 <- as.vector(test_pred0)
   testlabel1 <- as.vector(testlabel)
   if (svmType == 2 | svmType == 4){
        y0 <- as.vector(y10)
        tmaxy <- max(y0, na.rm = TRUE)
        tminy <- min(y0, na.rm = TRUE)
        tdify <- tmaxy-tminy

        y3 <- test_pred1+1
        y2 <- y3*tdify
        y1 <- y2/2
        test_pred2 <- y1+tminy

        y_3 <- testlabel1+1
        y_2 <- y_3*tdify
        y_1 <- y_2/2
        testlabel2 <- y_1+tminy
    } else {
        y0 <- as.vector(y10)
        y1 <- unique(y0)
        y2 <- rank(y1)
        if (all(y1 == y2)) {
            test_pred2 <- test_pred1
            testlabel2 <- testlabel1
        } else {
            test_pred2 <- mapvec(test_pred1, y2, y1)
            testlabel2 <- mapvec(testlabel1, y2, y1)
        }
    }

    predict0 <- data.frame(rowid = rperm[(train_size+1):(train_size+test_size)], predictedlabel = test_pred2, testlabel = testlabel2)
    order0 <- order(predict0$rowid)
    prediction <- predict0[order0,]

    # test_error
    test_error0 <- result$test_error
    test_error1 <- test_error0[length(test_error0)]
    test_error <- data.frame(test_error = test_error1)
}

# rank
if (svmType == 3 | svmType == 4){
    theta0 <- result$theta
    theta1 <- theta0[-length(theta0)]
    order_0 <- order(theta1, decreasing = TRUE)
    order_1 <- order(order_0)
    rank <- data.frame(variable = olabel, rank = order_1, weight = theta1)
} 

if (svmType == 5 | svmType == 6){
    output <- list(best_subsets = best_subsets)
} else if (svmType == 3 | svmType == 4){
    output <- list(test_error = test_error, prediction = prediction, feature_rank = rank)
} else {
    output <- list(test_error = test_error, prediction = prediction)
}

return(output)

}
