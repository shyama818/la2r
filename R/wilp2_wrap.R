#' Conduct Wilcoxon Signed Rank test for ordered factor variable
#'
#' This function immplements wilcox.test with paired = TRUE after checking that each group in the input dataset has at least 2 non-null records
#'
#' @param x0 dataframe
#' @param lhs A character string which is name of the main variable of ordered factor type
#' @param rhs A character string which is name of the group variable of factor type with two levels
#' @param paracode A character vector which is used for the specification of the 4 parameters (alternative, correct, conf.level, na.action).
#'    
#' @return an R List which contains one dataframe;  
#'    wil_signedrank_result.
#'
#' @examples
#' \dontrun{
#' wilp2_wrap(demo, "BMI", "Sex", c("two.sided", "TRUE", "0.95", "na.omit"))
#' }
#'
#' @export
wilp2_wrap <- function(x0, lhs, rhs, paracode) {
        output <- NULL
        spl1 <- as.integer(x0[,lhs])
        spl2 <- as.integer(x0[,rhs])
        if (length(na.omit(spl1)) > 1 & length(na.omit(spl2)) > 1){
            outtest <- wilcox.test(x = spl1, y = spl2, paired = TRUE, alternative = paracode[1], correct = as.logical(paracode[2]), conf.level = as.double(paracode[3]), na.action = paracode[4])
            ot1 <- data.frame(statistic=outtest$statistic, p.value=outtest$p.value, alternative=outtest$alternative, row.names = NULL)
            labels(ot1) <- c("the value of the test statistic", "the p-value for the test", "the alternative hypothesis")

            output <- list(wil_signedrank_result = ot1)
        }
        return(output)
}

