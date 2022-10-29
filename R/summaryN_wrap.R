#' Compute summary statistics of a categorical variable 
#'
#' @param x dataframe
#' @param var A character string which is name of the main variable of numeric type
#' @param exclude0 A character string which is used for the specification of the parameter exclude of the table function.
#'    
#' @return an R List which contains 1 dataframe;  
#'    summary_result.
#'
#' @examples
#' \dontrun{
#' summaryN_wrap(demo, "AgeCat", "c(NA, NaN)")
#' }
#'
#' @export
summaryN_wrap <- function(x, var, exclude0) {

    t1 <- table(x[,c(var)], exclude=exclude0)
    ot1 <- as.data.frame(t1)
    colnames(ot1) <- c(var,"N")

    output <- list(summary_result = ot1)
    return(output)

}

