#' Conduct Wilcoxon Rank Sum test for ordered factor variable
#'
#' This function immplements wilcox.test after checking that each group in the input dataset has at least 2 non-null records
#'
#' @param x0 dataframe
#' @param lhs A character string which is name of the main variable of ordered factor type
#' @param rhs A character string which is name of the group variable of factor type with two levels
#' @param paracode A character vector which is used for the specification of the 4 parameters (alternative, correct, conf.level, na.action).
#'    
#' @return an R List which contains two dataframes;  
#'    wil_ranksum_result and wil_ranksum_count.
#'
#' @examples
#' \dontrun{
#' wil2_wrap(demo, "BMI", "Sex", c("two.sided", "TRUE", "0.95", "na.omit"))
#' }
#'
#' @export
wil2_wrap <- function(x0, lhs, rhs, paracode) {
    spl <- split(x0[lhs], x0[rhs], drop = TRUE, lex.order = TRUE)
    output <- NULL
    if (length(spl) > 1) {
        spl1 <- as.integer(spl[[1]][,lhs])
        spl2 <- as.integer(spl[[2]][,lhs])
        if (length(na.omit(spl1)) > 1 & length(na.omit(spl2)) > 1){
            outtest <- wilcox.test(x = spl1, y = spl2, alternative = paracode[1], correct = as.logical(paracode[2]), conf.level = as.double(paracode[3]), na.action = paracode[4])
            ot1 <- data.frame(statistic=outtest$statistic, p.value=outtest$p.value, alternative=outtest$alternative, row.names = NULL)
            labels(ot1) <- c("the value of the test statistic", "the p-value for the test", "the alternative hypothesis")

            table01 <- table(x0[,lhs], x0[,rhs], exclude = c(NA, NaN))
            ot2<- as.data.frame(table01)
            colnames(ot2) <- c(lhs, rhs, "Frequency")
            ot2[,lhs] <- factor(ot2[,lhs], ordered = is.ordered(x0[,lhs]), levels = levels(x0[,lhs]))
            ot2[,rhs] <- factor(ot2[,rhs], ordered = is.ordered(x0[,rhs]), levels = levels(x0[,rhs]))

            output <- list(wil_ranksum_result = ot1, wil_ranksum_count = ot2)
        } 
    }
    return(output)
}

