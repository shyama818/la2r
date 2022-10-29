#' Count N  
#'
#' New version of the length function which can handle NA's: if na.rm==T, it doesn't count them
#'
#' @param x dataframe
#' @param na.rm a character string (TRUE or FALSE) for the parameter na.rm
#'    
#' @return N
#'
#' @examples
#' \dontrun{
#' length2(demo, na.rm=TRUE)
#' }d
# 
length2 <- function (x, na.rm=TRUE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
}

