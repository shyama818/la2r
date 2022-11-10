#' output a dataframe which contains all realization of the variables
#'
#' This function generate a dataframe which contains all realization of the variables given by colStrings
#'
#' @param dsgroup dataframe
#' @param colStrings A character vector eash of which is name of the variables
#'
#' @return a dataframe which contains all realization of the variables
#'
#' @examples
#' \dontrun{
#' table_strat_s1(demo, c("Ethnic", "Sex", "AgeCat", "Treatment"))
#' }
#'
#' @export
table_strat_s1 <- function(dsgroup, colStrings){

colV <- dstring(colStrings)
colVars <- colV(dsgroup)

ap1 <- split(dsgroup, colVars, drop = FALSE, sep = ";,;", lex.order = TRUE)
bp1 <- attributes(ap1)

outmat <- matrix(nrow=length(ap1),ncol=length(colStrings))
for(i0 in 1:length(ap1)) {
    if (length(colStrings) == 1) {
            outmat[i0,] <- bp1$names[i0]
    } else {
            outmat[i0,] <- mystrsplit(bp1$names[i0], ";,;")
    }
}

# make data.frame
ingroup <- dsgroup[,colStrings, drop = FALSE]
output2 <- simple_mat(ingroup, outmat)
return(output2)

}

