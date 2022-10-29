#' apply a summary statistic to a numeric vector and apply a format function to the result
#'
#' apply a summary statistic (N, sum, mean, median, min, max, mode, sd, se, meanSD, meanSE) to a numeric vector
#' and apply a round function and a format function to the result if type = 1.
#' check uniquness; then 1. apply a round and format function or 2. return the input value as it is if type = 2. 
#'
#' @param vpl a numeric vector
#' @param type a numeric string: 1 Type 1 for Base dataset, 2 Type 2 for the output dataset of Stats or ML node
#' @param statString a numeric string: choose from (N, sum, mean, median, min, max, mode, sd, se, meanSD, meanSE)
#' @param narmString a character string: TRUE or FALSE for na.rm parameter
#' @param digitString a numeric string: the minimum number of digits to the right of the decimal point
#' @param digitString2 a numeric string: the minimum number of digits to the right of the decimal point for SD or SE in meanSD and meanSE
#'
#' @return a character string: result
#'
#' @examples
#' \dontrun{
#' stats_mp(vector0, 1, "meanSE", TRUE, 1, 2)
#' }
#'
stats_mp <- function(vpl, type, statString, narmString, digitString, digitString2){
    if (type==1){
        if (statString == "identity"){
            if (length(vpl) > 1){
                stop("There exist multiple records for each realisation of stratification, column and row variables.")
            } else {
                main0 <- format(round(vpl, as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString))
            }
        } else if (statString == "list"){
            main0 <- ""
            for (k0 in 1:length(vpl)){
                if (k0 == 1){
                    if (is.na(vpl[k0])){
                        main0 <- "NA"
                    } else {
                        main0 <- format(round(vpl[k0], as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString))
                    }  
                } else {
                    if (is.na(vpl[k0])){
                        main0 <- str_c(main0, "NA", sep = ", ")
                    } else {
                        main0 <- str_c(main0, format(round(vpl[k0], as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString)), sep = ", ")
                    }
                }   
            }
        } else if (statString == "N"){
            main0 <- length2(vpl, na.rm = narmString)
        } else if (statString == "sum"){
            main0 <- format(round(sum(vpl, na.rm = narmString), as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString))
        } else if (statString == "mean"){
            main0 <- format(round(mean(vpl, na.rm = narmString), as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString))
        } else if (statString == "median"){
            main0 <- format(round(median(vpl, na.rm = narmString), as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString))
        } else if (statString == "min"){
            main0 <- format(round(min(vpl, na.rm = narmString), as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString))
        } else if (statString == "max"){
            main0 <- format(round(max(vpl, na.rm = narmString), as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString))
        } else if (statString == "mode"){
            main0 <- mymode(vpl)
        } else if (statString == "sd"){
            main0 <- format(round(sd(vpl, na.rm = narmString), as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString))
        } else if (statString == "se"){
            main0 <- format(round(sd(vpl, na.rm = narmString)/sqrt(length2(vpl, na.rm=narmString)), as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString))
        } else if (statString == "meanSD"){
            if (length2(vpl, na.rm=narmString) > 1){
                main0 <- str_c(format(round(mean(vpl, na.rm = narmString), as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString)), format(round(sd(vpl, na.rm = narmString), as.integer(digitString2)), digits = as.integer(digitString2), nsmall = as.integer(digitString2)), sep = " +- ")
            } else {
                main0 <- format(round(mean(vpl, na.rm = narmString), as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString))
            }
        } else if (statString == "meanSE"){
            if (length2(vpl, na.rm=narmString) > 1){
                main0 <- str_c(format(round(mean(vpl, na.rm = narmString), as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString)), format(round(sd(vpl, na.rm = narmString)/sqrt(length2(vpl, na.rm=narmString)), as.integer(digitString2)), digits = as.integer(digitString2), nsmall = as.integer(digitString2)), sep = " +- ")
            } else {
                main0 <- format(round(mean(vpl, na.rm = narmString), as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString))
            }
        }  
    } else if (type==2) {
        if (length(vpl) > 1){
            stop("There exist multiple records for each realisation of stratification, column and row variables.")
        } else if (is.null(digitString)) {
            main0 <- vpl
        } else {
            main0 <- format(round(vpl, as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString))
        }
    }
    return(main0)
} 

