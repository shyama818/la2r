#' Check the uniqueness for the subset for each realization of the variables
#'
#' This function check that there is only a single record for the subset corresponding to each relization of the variables
#'
#' @param x dataframe
#' @param listStrings A character vector which are names of variables
#' @param type A numeric string: 1 error (stop the transaction), 2 warning (warning a message)
#' @param text A character string which is a message printed out
#'    
#'
#' @examples
#' \dontrun{
#' checkUnique(demo, c("BMI", "Sex"), 1, "There exist multiple records for each realisation of the variables.")
#' }
#'
#' @export
checkUnique <- function(x, listStrings, type, text) {
    if (length(listStrings) > 0){
    listV <- dstring(listStrings)
    listVars <- listV(x)
    ap0 <- split(x, listVars, drop = TRUE, sep = ";,;", lex.order = TRUE)
    for(gi0 in 1:length(ap0)) {
        spl <- ap0[[gi0]]    
        if (nrow(spl) > 1){
            if (type==1){
                stop(text)
            } else if (type==2){
                warning(text)
            } 
        }
    }
    }
}
