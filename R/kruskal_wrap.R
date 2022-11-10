#' Conduct Kruskal-Wallis test for the numeric variable 
#'
#' This function immplements kruskal.test after checking that each group in the input dataset has at least 2 non-null records 
#' and at least 1 record for each level of the factor of the group variable.
#'
#' @param x dataframe
#' @param lhs A character string which is name of the main variable of numeric type
#' @param rhs A character string which is name of the group variable of factor type
#' @param naaction A character vector which is used for the specification of the parameter na.action.
#'    
#' @return an R List which contains 1 dataframe;  
#'    kruskal_result.
#'
#' @examples
#' \dontrun{
#' kruskal_wrap(demo, "BMI", "AgeCat", "na.omit")
#' }
#'
#' @export
kruskal_wrap <- function(x, lhs, rhs, naaction) {

    l0 <- levels(x[,rhs])
    l1 <- length(l0)
    spl <- split(x[lhs], x[rhs], drop = TRUE, lex.order = TRUE)
    mcon1 <- TRUE
    for (i in 1:length(spl)){
        spli <- spl[[i]][,lhs]
        if (!(length(na.omit(spli)) > 1)){
            mcon1 <- FALSE
        } 
    }
    output <- NULL
    if (length(spl) == l1 & mcon1 == TRUE) {
        outtest <- kruskal.test(reformulate(rhs, lhs), data=x, na.action = naaction)
        ot1 <- data.frame(statistic=outtest$statistic, parameter=outtest$parameter, p.value=outtest$p.value, row.names = NULL)
        labels(ot1) <- c("the Kruskal-Wallis rank sum statistic", "the degrees of freedom of the approximate chi-squared distribution", "the p-value for the test")

        output <- list(kruskal_result = ot1)
    }
    return(output)
}

