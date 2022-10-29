#' Conduct survival analysis 
#'
#' @param x0 dataframe
#' @param lhs A character string which is name of the variable of numeric type for Time
#' @param rhs A character string which is name of the variable of numeric type for Event (1=target event, 0=censor)
#' @param rhs2 A character string which is name of the group variable of factor type
#' @param paracode A character vector which is used for the specification of the parameter na.action.
#'    
#' @return an R List which contains 2 dataframes;  
#'    survfit_result and survdiff_result.
#'
#' @examples
#' \dontrun{
#' survival_wrap(x, "Time", "Event", "AgeCat", c("na.omit"))
#' }
#'
#' @export
survival_wrap <- function(x0, lhs, rhs, rhs2, paracode) {

    if (!is.null(rhs2)){
        checkStrings <- c(lhs,rhs,rhs2)
    } else {
        checkStrings <- c(lhs,rhs)
    }
    check <- TRUE
            limit0 <- 2
            if (nrow(na.omit(x0[, checkStrings])) < limit0) {
                check <- FALSE
            }

    output <- NULL
    if (check){
    surv_object <- Surv(time = x0[,lhs], event = x0[,rhs])
    if (!is.null(rhs2)){

        outtest <- survfit(surv_object ~ x0[rhs2][[1]], data = x0, na.action = paracode[1])

        if (!is.null(outtest$strata)) {
            strata0 <- rep(str_sub(names(outtest$strata[1]), start = 15), outtest$strata[1])
            if (length(outtest$strata)>1){
            for (i in 2:length(outtest$strata)){
                strata0 <- c(strata0, rep(str_sub(names(outtest$strata[i]), start = 15), outtest$strata[i]))
            }
            }
            ot1 <- data.frame(strata=strata0, time=outtest$time, n.risk=outtest$n.risk, n.event=outtest$n.event, n.censor=outtest$n.censor, surv=outtest$surv, std.err=outtest$std.err, lower=outtest$lower, upper=outtest$upper, row.names = NULL)
            labels(ot1) <- c("strata", "time", "number at risk", "number of event", "number of censor", "survival ratio", "standard error", "lower 95% CI", "upper 95% CI")

            surv_diff <- survdiff(surv_object ~ x0[rhs2][[1]], data = x0, na.action = paracode[1])
            ot2 <- data.frame(statistic=surv_diff$chisq, parameter=length(surv_diff$n)-1, p.value=pchisq(surv_diff$chisq, length(surv_diff$n)-1, lower.tail = FALSE), row.names = NULL)
            labels(ot2) <- c("the chisquare statistic for a test of equality", "the degrees of freedom", "the p-value for the test")
        } else {
            ot1 <- data.frame(strata=rep(na.omit(x0[,rhs2])[1],length(outtest$time)), time=outtest$time, n.risk=outtest$n.risk, n.event=outtest$n.event, n.censor=outtest$n.censor, surv=outtest$surv, std.err=outtest$std.err, 
lower=outtest$lower, upper=outtest$upper, row.names = NULL)
        labels(ot1) <- c("strata", "time", "number at risk", "number of event", "number of censor", "survival ratio", "standard error", "lower 95% CI", "upper 95% CI")

            ot2 <- NULL
        }
    } else {
        fit1 <- survfit(surv_object ~ 1, data = x0, na.action = paracode[1])

        outtest <- summary(fit1)

        ot1 <- data.frame(strata=rep("all",length(outtest$time)), time=outtest$time, n.risk=outtest$n.risk, n.event=outtest$n.event, n.censor=outtest$n.censor, surv=outtest$surv, std.err=outtest$std.err, lower=outtest$lower, upper=outtest$upper, row.names = NULL)
        labels(ot1) <- c("strata", "time", "number at risk", "number of event", "number of censor", "survival ratio", "standard error", "lower 95% CI", "upper 95% CI")

        ot2 <- NULL

    }

    output <- list(survfit_result = ot1, survdiff_result = ot2)

    }
    return(output)
}

