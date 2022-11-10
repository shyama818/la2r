#' Comput summary statistics
#'
#' Create a dataframe whose variables are mean, sd, median, min, max, q1 and q3.
#'
#' @param x dataframe
#' @param var a character string which is the name of a variable
#' @param na.rm0 a character string (TRUE or FALSE) for the parameter na.rm
#'    
#' @return the dataframeof summary statistics.
#'
#' @examples
#' \dontrun{
#' summary0(x, "Age", TRUE)
#' }
#'
#' @export
summary0 <- function(x, var, na.rm0) {

    ot1 <- data.frame(N = length2(x[[var]], na.rm=na.rm0),
          mean = mean(x[[var]], na.rm=na.rm0),
          sd = sd(x[[var]], na.rm=na.rm0),
          median = median(x[[var]], na.rm=na.rm0),
          min = min(x[[var]], na.rm=na.rm0),
          max = max(x[[var]], na.rm=na.rm0),
          q1 = quantile(x[[var]], probs = 0.25, na.rm=na.rm0),
          q3 = quantile(x[[var]], probs = 0.75, na.rm=na.rm0),
        row.names = NULL)
    ot1$se <- ot1$sd/sqrt(ot1$N)

    return(ot1)

}

