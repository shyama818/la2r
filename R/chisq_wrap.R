#' Conduct chi-square test 
#'
#' This function immplements chisq.test after checking that the number of the row and the number of the column of the contingency table 
#' of the input dataset are both at least 2.
#'
#' @param x0 dataframe
#' @param lhs A character string which is name of the main variable of factor type
#' @param rhs A character string which is name of the group variable of factor type
#' @param correct0 A character vector which is used for the specification of the parameter correct.
#'    
#' @return an R List which contains 3 dataframes;  
#'    chitest_result, chitest_count and chitest_check.
#'
#' @examples
#' \dontrun{
#' chisq_wrap(demo, "Sex", "AgeCat", "TRUE")
#' }
#'
#' @export
chisq_wrap <- function(x0, lhs, rhs, correct0) {

    table00 <- table(as.integer(x0[,lhs]), as.integer(x0[,rhs]), exclude = c(NA, NaN))
    kr0 <- nrow(table00)
    kc0 <- ncol(table00)
    table01 <- table(x0[,lhs], x0[,rhs], exclude = c(NA, NaN))
    kr <- nrow(table01)
    kc <- ncol(table01)
    output <- NULL
    if (kr0 > 1 & kc0 > 1) {
    chi01 <- chisq.test(x=table01, correct = correct0)
 
    ot1 <- data.frame(statistic=chi01$statistic, parameter=chi01$parameter, p.value=chi01$p.value, row.names = NULL)
    labels(ot1) <- c("the value of the chi-squared test", "the degrees of freedom of the approximate chi-squared distribution", "the p-value for the test")

    ot10 <- chi01$observed
    ot10B <- as.table(ot10,nrow=kr,ncol=kc,dimnames=dimnames(table01))
    ot11 <- as.data.frame(ot10)
    colnames(ot11) <- c(lhs, rhs, "observed")

    ot20 <- chi01$expected
    ot20B <- as.table(ot20,nrow=kr,ncol=kc,dimnames=dimnames(table01))
    ot21 <- as.data.frame(ot20B)
    colnames(ot21) <- c(lhs, rhs, "expected")

    ot30 <- chi01$residuals
    ot30B <- as.table(ot30,nrow=kr,ncol=kc,dimnames=dimnames(table01))
    ot31 <- as.data.frame(ot30B)
    colnames(ot31) <- c(lhs, rhs, "residuals")

    ot40 <- chi01$stdres
    ot40B <- as.table(ot40,nrow=kr,ncol=kc,dimnames=dimnames(table01))
    ot41 <- as.data.frame(ot40B)
    colnames(ot41) <- c(lhs, rhs, "std.residuals")

    ot2 <- data.frame(ot11, expected=ot21[,"expected"], residuals=ot31[,"residuals"], std.residuals=ot41[,"std.residuals"])
    labels(ot2) <- c(lhs, rhs, "the observed counts", "the expected counts under the null hypothesis", "the Pearson residuals: (observed-expected)/sqrt(expected)", "the standardized residuals")

    ck01 <- as.vector(ot20)
    ck02 <- which(ck01>=5)
    ratio <- length(ck02)/length(ck01)
    ot3 <- data.frame(ratio)
    if (ratio >= 0.8){
                ot3$check <- TRUE
            } else {
                ot3$check <- FALSE
            }
    labels(ot3) <- c("the ratio of the expected values which are greater than or equal to 5", "TRUE if at least 80 percent of the expected values are at least 5.")

    output <- list(chitest_result = ot1, chitest_count = ot2, chitest_check = ot3)
    }
    return(output)

}

