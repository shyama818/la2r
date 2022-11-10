#' convert a matrix which is the first object of a list to a dataframe
#'
#' convert a matrix which is the first object of a list to a dataframe
#'
#' @param idata list
#'
#' @return dataframe
#'
#' @examples
#' \dontrun{
#' mat2data(adj)
#' }
#'
#' @export
mat2data <- function(idata){
  if (is.data.frame(idata) == FALSE && is.list(idata) == TRUE){
    imat <- idata[[1]]
    if (is.matrix(imat) == TRUE){
        col_name <- colnames(imat)
        if (is.null(col_name)){
            coln <- ncol(imat)
            colnames(imat) <- paste0("col",as.character(1:coln))
        }
    }
    idata <- as.data.frame(imat)
  }
  return(idata)
}
