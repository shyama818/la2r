#' Conduct paired t test  
#'
#' This function immplements t.test with the parameter paired = TRUE after checking that each group in the input dataset has at least 2 non-null records.
#'
#' @param x0 dataframe
#' @param lhs A character string which is name of the main variable of numeric type
#' @param rhs A character string which is name of the group variable of factor type with two levels
#' @param paracode A character vector which is used for the specification of the 4 parameters (alternative, var.equal, conf.level, na.action).
#'    
#' @return an R List which contains one dataframe;  
#'    pttest_result.
#'
#' @examples
#' \dontrun{
#' pttest_wrap(demo, "BMI", "Sex", c("two.sided", "TRUE", "0.95", "na.omit"))
#' }
#'
#' @export
pttest_wrap <- function(x0, lhs, rhs, paracode) {
        output <- NULL
        spl1 <- x0[,lhs]
        spl2 <- x0[,rhs]
        if (length(na.omit(spl1)) > 1 & length(na.omit(spl2)) > 1){
            outtest <- t.test(x = spl1, y = spl2, paired = TRUE, alternative = paracode[1], var.equal = as.logical(paracode[2]), conf.level = as.double(paracode[3]), na.action = paracode[4])
            ot1 <- data.frame(statistic=outtest$statistic, parameter=outtest$parameter, p.value=outtest$p.value, conf.int.min=outtest$conf.int[1], conf.int.max=outtest$conf.int[2], mean=outtest$estimate, null.value=outtest$null.value, stderr=outtest$stderr, alternative=outtest$alternative, row.names = NULL)
labels(ot1) <- c("the value of the t-statistic", "the degrees of freedom for the t-statistic", "the p-value for the test", "the lower limit of the confidence interval", "the upper limit of the confidence interva", "mean of the differences", "mean difference", "the standard error of the mean difference", "the alternative hypothesis")

            output <- list(pttest_result = ot1)
            return(output)
        }
}

