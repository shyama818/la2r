#' For each realization of the variables (listStrings), call the stats_mp function for the variable (mainString)
#'
#' For each realization of the variables (listStrings),
#' apply the stats_mp function to the variable (mainString).
#'
#' @param dsgroup dataframe
#' @param listStrings A character vector of the names of variables
#' @param mainString a numeric string: the name of the variable
#' @param type a numeric string: 1 Type 1 for Base dataset, 2 Type 2 for the output dataset of Stats or ML node
#' @param statString a numeric string: choose from (N, sum, mean, median, min, max, mode, sd, se, meanSD, meanSE)
#' @param narmString a character string: TRUE or FALSE for na.rm parameter
#' @param digitString a numeric string: the minimum number of digits to the right of the decimal point
#' @param digitString2 a numeric string: the minimum number of digits to the right of the decimal point for SD or SE in meanSD and meanSE
#'
#' @return a character string
#'
#' @examples
#' \dontrun{
#' data_mp(demo, c("Treatment", "Sex", "Ethnic"), BMI, 1, "meanSE", TRUE, 1, 2)
#' }
#'
#' @export
data_mp <- function(dsgroup, listStrings, mainString, type, statString, narmString, digitString, digitString2){
if (is.null(listStrings)){
    spl <- dsgroup[, mainString]
    vpl <- as.vector(unlist(spl))   
    main1 <- stats_mp(vpl, type, statString, narmString, digitString, digitString2)
    output1 <- data.frame(main.var=main1, stringsAsFactors = FALSE)
} else {
    listV <- dstring(listStrings)
    listVars <- listV(dsgroup)
    ap0 <- split(dsgroup, listVars, drop = TRUE, sep = ";,;", lex.order = TRUE)
    bp0 <- attributes(ap0)
    outmat0 <- matrix(nrow=length(ap0),ncol=length(listStrings))
    main1 <- vector(length=length(ap0))
    for(gi0 in 1:length(ap0)) {
        if (length(listStrings) == 1) {
            outmat0[gi0,] <- bp0$names[gi0]
        } else {
            outmat0[gi0,] <- mystrsplit(bp0$names[gi0], ";,;")
        }
        spl <- ap0[[gi0]][, mainString]
        vpl <- as.vector(unlist(spl))   
        main1[gi0] <- stats_mp(vpl, type, statString, narmString, digitString, digitString2)
    }
ingroup <- dsgroup[,listStrings, drop = FALSE]
output0 <- simple_mat(ingroup, outmat0)
output1 <- data.frame(output0, main.var=main1, stringsAsFactors = FALSE)
}
return(output1)
}

