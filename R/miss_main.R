#' Create a dataframe with no missing values
#'
#' Run the classification and the regresson methods in
#' "Scalable Kernel Methods via Doubly Stochastic Gradients".
#'
#' @param dsgroup dataframe: input dataframe
#' @param xNames A string vector: the names of the input variables
#' @param yName A string: the name of the target variable
#' @param vec An integer vector (size of xNames): type of input variables (1 numerical variable, 2 ordered factor variable, 3 un-ordered factor variable)
#' @param missType An integer: 1 To remove the records with any missing values, 
#' 2 to replace the missing values with the mean (for the continuous variable) and the mode (for the categorical variable)
#'
#' @return dataframe of the input variables and the target variable with no missing values  
#'
#' @examples
#' \dontrun{
#' miss_main(demo,c("Bssys","Bsdia","Blwt","Blht","AgeCat","Wt2Cls","Sex","treatment","Elig","Crclct"), c("BMI"), c(1,1,1,1,2,2,3,3,3,3), 1)
#' }
#'
#' @export
miss_main <- function(dsgroup, xNames, yName, vec, missType){

data00 <- dsgroup 
datam <- data.matrix(data00)
y01 <- datam[, yName, drop = FALSE]

    # Delete Missing Y

        n1 <- nrow(y01)

        naF <- c()
        for (i in 1:n1){
            naF0 <- 0 
            if (is.na(y01[i,1])){
                naF0 <- 1
            }
            if (naF0==1) naF <- c(naF,-i)
        }
        if (length(naF)>0){
            data01 <- data00[naF,,drop = FALSE]
        } else {
            data01 <- data00
        }

    datam1 <- data.matrix(data01)
    x01 <- datam1[, xNames, drop = FALSE]

    ntc <- ncol(x01)
    ntr <- nrow(x01)

    if (missType == 1){

        naF <- c()
        for (i in 1:ntr){
            naF0 <- 0
            for (j in 1:ntc){  
                if (is.na(x01[i,j])){
                    naF0 <- 1
                }
            }
            if (naF0==1) naF <- c(naF,-i)
        }
        if (length(naF)>0){
            data02 <- data01[naF,,drop = FALSE]
        } else {
            data02 <- data01
        }

    } else {

        # Apply mean or mode to the missing data
        # na matrix

        namat <- matrix(0, ntr, ntc) 

        for (i in 1:ntr){
            for (j in 1:ntc){
                if (is.na(x01[i,j])) namat[i,j] <- 1
            }
        }

        # number of NA at each column

        nacol <- colSums(namat)

        # apply mean or mode to the missing data

        for (i in 1:ntc){
            if (nacol[i] > 0) {
                w1 <- which(namat[,i]==1)
                w2 <- which(namat[,i]==0)
                v0 <- x01[,i][w2]
                for (k in 1:nacol[i]){
                    if (vec[i]==1) {
                        x01[w1[k],i] <- mean(v0)
                        data01[w1[k],xNames[i]] <- mean(v0)
                    } else {
                        x01[w1[k],i] <- mymode(v0)
                        if (is.factor(data01[,xNames[i]])){
                            level0 <- levels(data01[,xNames[i]])
                            data01[w1[k],xNames[i]] <- level0[mymode(v0)]
                        } else {
                            data01[w1[k],xNames[i]] <- mymode(v0)
                        }
             
                    }
                }
            }
        }
        data02 <- data01
    }

    return(data02)

}

