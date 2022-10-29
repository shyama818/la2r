#' Compute summary statistics of a continuous variable 
#'
#' @param x dataframe
#' @param var A character string which is name of the main variable of numeric type
#' @param na.rm0 A character string which is used for the specification of the parameter na.rm.
#'    
#' @return an R List which contains 1 dataframe;  
#'    summary_result.
#'
#' @examples
#' \dontrun{
#' summary_wrap(demo, "BMI", TRUE)
#' }
#'
#' @export
summary_wrap <- function(x, var, na.rm0) {

    ot1 <- summary0(x, var, na.rm0)

    labels(ot1) <- c("N", "Mean", "Standard Deviation", "Median", "Min", "Max", "Q1", "Q3", "Standard Error")

     output <- list(summary_result = ot1)
     return(output)

}

