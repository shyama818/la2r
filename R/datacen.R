#' Normalize the numeric variable
#'
#' This function normalizes the numeric variable by computing: 
#' the normalized variable = the numeric variable (lhs) - the mean of the numeric variable (lhs) for each value of the group variable (rhs)
#'
#' @param dsgroup dataframe
#' @param lhs A character string which is name of the main variable of numeric type
#' @param rhs A character string which is name of the group variable of factor type
#'    
#' @return the normalized variable
#'
#' @examples
#' \dontrun{
#' datacen(demo, "BMI", "SEX")
#' }
#'
datacen <- function(dsgroup, lhs, rhs){
    datac <- ddply(dsgroup, c(rhs), 
        .fun = function(xxx) { 
        summary0(xxx, lhs, TRUE) 
    })
    means0 <- datac[,"mean"]
    meancen <- dsgroup[,lhs]-means0[as.numeric(dsgroup[,rhs])]
    return(meancen)
}
