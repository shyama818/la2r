#' Run the PSO method
#'
#' Run the PSO method in 
#' "A novel binary particle swarm optimization"
#' based on the classification and the regresson methods in
#' "Scalable Kernel Methods via Doubly Stochastic Gradients".
#'
#' @param cache_n An integer: The best N Feature Subsets generated by PSO
#' @param pop_n An integer: the population size of the PSO
#' @param gen_n An integer: the iteration size of the PSO
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
#' pso_com(20, 10, 5, 1e-5, 1, trainlabel, traindata, testlabel, testdata, 10, c(1, 2, 3, 4, 6, 10), 1)
#' }
#'
#' @export
pso_com <- function(cache_n,pop_n,gen_n,reg_param,ite_n,trainlabel,traindata,testlabel,testdata,odimno,start,c0type){

m1 <- odimno
ntr <- ncol(traindata)
nte <- ncol(testdata)

# BPSO
w0 <- 0.729
c1 <- 1.49
c2 <- 1.49
EPS0S <- 0.08
EPS0E <- 0.01

ite <- 1
vrat <- 1

set.seed(as.integer(Sys.time()))
x0 <- matrix(runif(pop_n*m1),pop_n,m1)
v <- 0.1*x0

# x - middle of the grid
x <- matrix(0,pop_n,m1)
for (i in 1:pop_n){
    flg_zero <- 0
    for (j in 1:m1){
        if (runif(1)<=x0[i,j]){
            x[i,j] <- 1
        }else{
            x[i,j] <- 0
        }
		if (x[i,j]==0){
            flg_zero <- flg_zero + 1
		}
     }
     if (flg_zero==m1){
        r1 <- sample.int(m1, 1)
        x[i,r1] <- 1
     }
}

hs_x <- uniqueVec(x)
hs_n <- nrow(hs_x)

hs_f <- matrix(0,hs_n,1)
for (i in 1:hs_n){
    T1 <- paste("-pso-iteration:", ite, ",", "particle:", i, sep = " ")
    print(T1)
    hsx <- hs_x[i,,drop = FALSE]
    hs_f[i,1] <- f_rbf(hsx, reg_param, ite_n, trainlabel, traindata, testlabel, testdata, start, c0type)
}

f0 <- matrix(0,pop_n,1)
for (i in 1:pop_n) {
    index_h0 <- row_find(hs_x,x[i,])+1
    f0[i,1] <- hs_f[index_h0,1]
}

fmin0 <- min(f0)
index_g0 <- which.min(f0)

pbest <- x
gbest <- x[index_g0,,drop = FALSE] 

while (ite < gen_n){

ite <- ite+1

kite <- (gen_n-ite)/gen_n
EPS <- EPS0E+(EPS0S-EPS0E)*kite

set.seed(as.integer(Sys.time()))
# pso velocity updates
for (i in 1:pop_n){
    for (j in 1:m1){
        v[i,j] <- w0*v[i,j]+c1*runif(1)*(pbest[i,j]-x0[i,j])+c2*runif(1)*(gbest[1,j]-x0[i,j])
    }
}

# pso position update
for (i in 1:pop_n){
    for (j in 1:m1){
        x0[i,j] <- x0[i,j]+vrat*v[i,j]
    }
}

# handling boundary violations
for (i in 1:pop_n){
    for (j in 1:m1){
        if (x0[i,j]<0+EPS){
            x0[i,j] <- 0+EPS
        } else if (x0[i,j]>1-EPS){
            x0[i,j] <- 1-EPS
        }
    }
}

# x
x <- matrix(0,pop_n,m1)
for (i in 1:pop_n){
   flg_zero <- 0
   for (j in 1:m1){
       if (runif(1)<=x0[i,j]){
            x[i,j] <- 1
       }else{
            x[i,j] <- 0
       }
	   if (x[i,j]==0){
            flg_zero <- flg_zero + 1
	   }
    }
    if (flg_zero==m1){
        r1 <- sample.int(m1,1)
        x[i,r1] <- 1
    }
}

f <- matrix(0,pop_n,1)
for (i in 1:pop_n){
    T1 <- paste("-pso-iteration:", ite, ",", "particle:", i, sep = " ")
    print(T1)
    ind_p0 <- row_find(hs_x,x[i,])+1
    ind_p0
    if (ind_p0==0){
        sx <- x[i,,drop = FALSE]
        f[i,1] <- f_rbf(sx, reg_param, ite_n, trainlabel, traindata, testlabel, testdata, start, c0type)
        hs_x <- rbind(hs_x,sx)
        sf <- f[i,1,drop = FALSE]
        hs_f <- rbind(hs_f,sf)
        hs_n <- hs_n+1
    }else{
        f[i,1] <- hs_f[ind_p0,1]
    }
}

for (i in 1:pop_n){
    if (f[i,1]<f0[i,1]){
        pbest[i,1] <- x[i,1]
        f0[i,1] <- f[i,1]
    }
}

fmin <- min(f0)
mindex <- which.min(f0)

# updating gbest and best fitness
if (fmin<fmin0){
    gbest <- pbest[mindex,,drop = FALSE]
    fmin0 <- fmin
}

if (cache_n > 0){
    fI <- order(hs_f)
    if (nrow(hs_f) > cache_n){
		hs_f <- hs_f[fI[1:cache_n],,drop = FALSE]
		hs_x <- hs_x[fI[1:cache_n],,drop = FALSE]
    } else {
        hs_f <- hs_f[fI,,drop = FALSE]
		hs_x <- hs_x[fI,,drop = FALSE]
    }
}

}

pso_list <- list(best_results = hs_f, best_subsets = hs_x)

}
