#' convert matrix to dataframe
#'
#' convert matrix x to dataframe  
#' If the name of each column of x is contained in the dataframe (indata), 
#' it is converted to the same type of the variable in indata.
#'
#' @param indata dataframe
#' @param inmat matrix
#'
#' @return dataframe
#'
#' @examples
#' \dontrun{
#' simple_mat(adj, inmat)
#' }
#'
simple_mat <- function(indata, inmat){

col_name <- colnames(indata)
col_type <- rep(NA,length(col_name))
factor_type <- rep(NA,length(col_name))
double_type <- rep(NA,length(col_name))
integer_type <- rep(NA,length(col_name))
logical_type <- rep(NA,length(col_name))
character_type <- rep(NA,length(col_name))
complex_type <- rep(NA,length(col_name))
for (i in 1:length(col_name)) {
    factor_type[i] <- is.factor(unlist(indata[col_name[i]]))
    double_type[i] <- is.double(unlist(indata[col_name[i]]))
    integer_type[i] <- is.integer(unlist(indata[col_name[i]]))
    logical_type[i] <- is.logical(unlist(indata[col_name[i]]))
    character_type[i] <- is.character(unlist(indata[col_name[i]]))
    complex_type[i] <- is.complex(unlist(indata[col_name[i]]))
}

outdata <- NULL
for (i in 1:ncol(indata)) {
    if (i==1){
        if (factor_type[i] == TRUE){
            outdata <- data.frame(factor(inmat[,i], ordered = is.ordered(indata[col_name[i]][,1]), levels = levels(indata[col_name[i]][,1])))
        } else if (double_type[i] == TRUE){
            outdata <- data.frame(as.double(inmat[,i]))
        } else if (integer_type[i] == TRUE){
            outdata <- data.frame(as.integer(inmat[,i]))
        } else if (logical_type[i] == TRUE){
            outdata <- data.frame(as.logical(inmat[,i]))
        } else if (character_type[i] == TRUE){
            outdata <- data.frame(as.character(inmat[,i]))
        } else if (complex_type[i] == TRUE){
            outdata <- data.frame(as.complex(inmat[,i]))
        }
    } else {
        if (factor_type[i] == TRUE){
            outdata <- data.frame(outdata,factor(inmat[,i], ordered = is.ordered(indata[col_name[i]][,1]), levels = levels(indata[col_name[i]][,1])))
        } else if (double_type[i] == TRUE){
            outdata <- data.frame(outdata,as.double(inmat[,i]))
        } else if (integer_type[i] == TRUE){
            outdata <- data.frame(outdata,as.integer(inmat[,i]))
        } else if (logical_type[i] == TRUE){
            outdata <- data.frame(outdata,as.logical(inmat[,i]))
        } else if (character_type[i] == TRUE){
            outdata <- data.frame(outdata,as.character(inmat[,i]))
        } else if (complex_type[i] == TRUE){
            outdata <- data.frame(outdata,as.complex(inmat[,i]))
        }
    }   
}
colnames(outdata) <- col_name

return(outdata)

}

