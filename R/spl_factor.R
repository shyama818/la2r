#' One-hot Encoding Scheme
#'
#' Apply the one-hot Encoding Scheme to the un-ordered factor variables
#'
#' @param dsg0 dataframe: input dataframe
#' @param vec An integer vector (size of xNames): type of input variables (1 numerical variable, 2 ordered factor variable, 3 un-ordered factor variable)
#'
#' @return R List with the following 4 objects  
#' 1. dsg1 dataframe after One-hot Encoding Scheme
#' 2. start0 a vactor (same size as the number of the columns of dsg1): 
#'               1 columns with no application of the one-hot Encoding Scheme, 0 columns created by the one-hot Encoding Scheme
#' 3. start1 a vactor (same size as the number of the columns of dsg1): a vector with the names of the original columns
#' 4. start2 a vactor (same size as the number of the columns of dsg1): a vector with the names of the new columns
#'
#' @examples
#' \dontrun{
#' spl_factor(demo, c(1,1,1,1,2,2,3,3,3,3))
#' }
#' 
spl_factor <- function(dsg0, vec){

columnNames0 <- colnames(dsg0)
dsg1 <- dsg0[,1,drop=FALSE]
start0 <- c()
start1 <- c()
start2 <- c()
for (gk in 1:ncol(dsg0)){
x_01 <- dsg0[,gk]
if (is.factor(x_01)){
    x_02 <- levels(x_01)
    levn <- length(x_02)
    if (levn > 2 & vec[gk]==3){
        for (gi in 1:levn){
            temp <- ifelse(x_01==x_02[gi],x_02[gi],str_c("not",x_02[gi],sep = " "))
            Levels0 = c( str_c("not",x_02[gi],sep = " "), x_02[gi])
            y <- factor(temp, levels = Levels0)
            if (length(unique(y)) > 1){
                dsg1 <- cbind(dsg1,y)
                colnames(dsg1)[ncol(dsg1)] <- str_c(columnNames0[gk],gi,sep = "___")
                start0 <- c(start0,0)
                start1 <- c(start1,columnNames0[gk])
                start2 <- c(start2,str_c(columnNames0[gk],gi,sep = "___"))
            }
        }
    } else {
        dsg1 <- cbind(dsg1,dsg0[,gk])
        colnames(dsg1)[ncol(dsg1)] <- columnNames0[gk]
        start0 <- c(start0,1)
        start1 <- c(start1,columnNames0[gk])
        start2 <- c(start2,columnNames0[gk])
    }
} else {
    dsg1 <- cbind(dsg1,dsg0[,gk])
    colnames(dsg1)[ncol(dsg1)] <- columnNames0[gk]
    start0 <- c(start0,1)
    start1 <- c(start1,columnNames0[gk])
    start2 <- c(start2,columnNames0[gk])
}
}
dsg1 <- dsg1[,-1]

result = list(data = dsg1, start0 = start0, start1 = start1, start2 = start2)
return(result)

}

