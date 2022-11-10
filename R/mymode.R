#' Comput mode
#'
#' Comput mode which is the most frequent of all values of a input vector
#'
#' @param v vector
#'    
#' @return mode
#'
#' @examples
#' \dontrun{
#' mymode(v)
#' }
#'
#' @export
mymode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

