#' Conduct anova test  
#'
#' This function immplements aov and Anova in car package after checking that each group in the input dataset has at least 2 non-null records 
#' and at least 1 record for each level of the factor of the group variable.
#'
#' @param x dataframe
#' @param lhs A character string which is name of the main variable of numeric type
#' @param rhs A character string which is name of the group variable of factor type with multiple levels
#' @param paracode A character string which is used for the specification of the parameter na.action.
#'    
#' @return an R List which contains 4 dataframes;  
#'    anova1_type1, anova1_type2, anova1_type3 and anova1_equalVariance_check.
#'
#' @examples
#' \dontrun{
#' anova1_wrap(demo, "BMI", "AGECAT", "na.omit")
#' }
#'
#' @export
anova1_wrap <- function(x, lhs, rhs, paracode) {

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
        result <- aov(lhs~rhs, data=x, na.action = paracode)

        sum <- summary(result)
        type10 <- sum[[1]]
        ot1 <- data.frame(statistic=type10[, "F value"][1], p.value=type10[, "Pr(>F)"][1], SS1=type10[, "Sum Sq"][1], parameter1=type10[, "Df"][1], MS1=type10[, "Mean Sq"][1], SS2=type10[, "Sum Sq"][2], parameter2=type10[, "Df"][2], MS2=type10[, "Mean Sq"][2], row.names = NULL)
labels(ot1) <- c("the value of the F-statistic (Type I test)", "the p-value for the test (Type I test)", str_c("sum-of-squares for ",rhs,sep = ""), str_c("the degrees of freedom for ",rhs,sep = ""), str_c("mean square for ",rhs,sep = ""), "sum-of-squares for residuals", "the degrees of freedom for residuals", "mean square for residuals")

        type20 <- Anova(result, type = "II")
        ot2 <- data.frame(statistic=type20[, "F value"][1], p.value=type20[, "Pr(>F)"][1], SS1=type20[, "Sum Sq"][1], parameter1=type20[, "Df"][1], SS2=type20[, "Sum Sq"][2], parameter2=type20[, "Df"][2], row.names = NULL)
labels(ot2) <- c("the value of the F-statistic (Type II test)", "the p-value (Type II test)", str_c("sum-of-squares for ",rhs,sep = ""), str_c("the degrees of freedom for ",rhs,sep = ""), "sum-of-squares for residuals", "the degrees of freedom for residuals")

        type30 <- Anova(result, type = "III")
        ot3 <- data.frame(statistic1=type30[, "F value"][1], p.value1=type30[, "Pr(>F)"][1], statistic2=type30[, "F value"][2], p.value2=type30[, "Pr(>F)"][2], SS1=type30[, "Sum Sq"][1], parameter1=type30[, "Df"][1], SS2=type30[, "Sum Sq"][2], parameter2=type30[, "Df"][2], SS3=type30[, "Sum Sq"][3], parameter3=type30[, "Df"][3], row.names = NULL)
labels(ot3) <- c("the value of the F-statistic for the intercept and the residuals (Type III test)", "the p-value for the intercept", str_c("the value of the F-statistic for ", rhs, " and the residuals (Type III test)", sep = ""), str_c("the p-value for ",rhs,sep = ""), "sum-of-squares for the intercept", "the degrees of freedom for the intercept", str_c("sum-of-squares for ",rhs,sep = ""), str_c("the degrees of freedom for ",rhs,sep = ""), "sum-of-squares for the residuals", "the degrees of freedom for the residuals")

    for (di in 1:length(spl)){
        sd.i <- sd(na.omit(spl[[di]][,lhs]))
        if (di == 1) {
            colname <- str_c("std",di,sep = "")
            lv1 <- data.frame(sd.i) 
            maxi <- sd.i
            mini <- sd.i
            colnames(lv1) <- c(colname)
        } else {
            colname <- str_c("std",di,sep = "")
            colname1 <- colnames(lv1)
            lv1 <- data.frame(lv1, sd.i) 
            if (sd.i > maxi) maxi <- sd.i
            if (sd.i < mini) mini <- sd.i
            colnames(lv1) <- c(colname1, colname)
        } 
    }

    colname0 <- c(colnames(lv1), "equalVarianceCheck")
    if (maxi/mini < 2){
        lv1$check0 <- TRUE
    } else {
        lv1$check0 <- FALSE
    }
    colnames(lv1) <- colname0
        
    output <- list(anova1_type1 = ot1, anova1_type2 = ot2, anova1_type3 = ot3, anova1_equalVariance_check = lv1)
    }
    return(output)
}

