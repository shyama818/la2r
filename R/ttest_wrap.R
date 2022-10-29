#' Conduct t test  
#'
#' This function immplements t.test after checking that each group in the input dataset has at least 2 non-null records.
#'
#' @param x0 dataframe
#' @param lhs A character string which is name of the main variable of numeric type
#' @param rhs A character string which is name of the group variable of factor type with two levels
#' @param paracode A character vector which is used for the specification of the 4 parameters (alternative, var.equal, conf.level, na.action).
#'    
#' @return an R List which contains two dataframes;  
#'    ttest_result and ttest_equalVariance_check.
#'
#' @examples
#' \dontrun{
#' ttest_wrap(x, "BMI", "Sex", c("two.sided", "TRUE", "0.95", "na.omit"))
#' }
#'
#' @export
ttest_wrap <- function(x0, lhs, rhs, paracode) {
    output <- NULL
    spl <- split(x0[lhs], x0[rhs], lex.order = TRUE)
    if (length(spl) > 1) {
        spl1 <- spl[[1]][,lhs]
        spl2 <- spl[[2]][,lhs]
        if (length(na.omit(spl1)) > 1 & length(na.omit(spl2)) > 1){
            outtest <- t.test(x = spl1, y = spl2, alternative = paracode[1], var.equal = as.logical(paracode[2]), conf.level = as.double(paracode[3]), na.action = paracode[4])
            ot1 <- data.frame(statistic=outtest$statistic, parameter=outtest$parameter, p.value=outtest$p.value, conf.int.min=outtest$conf.int[1], conf.int.max=outtest$conf.int[2], mean.x=outtest$estimate[1], mean.y=outtest$estimate[2], null.value=outtest$null.value, stderr=outtest$stderr, alternative=outtest$alternative, row.names = NULL)
labels(ot1) <- c("the value of the t-statistic", "the degrees of freedom for the t-statistic", "the p-value for the test", "the lower limit of the confidence interval", "the upper limit of the confidence interva", "mean of the group 1", "mean of the group 2", "mean difference", "the standard error of the mean difference", "the alternative hypothesis")
      
    sd.1 <- sd(na.omit(spl1))
    sd.2 <- sd(na.omit(spl2))
    lv1 <- data.frame(sd.1, sd.2)
    
    if (sd.1/sd.2 < 2 & sd.2/sd.1 < 2){
        lv1$check <- TRUE
    } else {
        lv1$check <- FALSE
    }
     
    labels(lv1) <- c("standard deviation of group 1", "standard deviation of group 2", "equalVariance_check")

            output <- list(ttest_result = ot1, ttest_equalVariance_check = lv1)
            
        } 
    }
    return(output)
}

