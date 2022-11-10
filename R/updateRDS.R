#' Update a dataframe
#'
#' Update a dataframe by deleting the variables which are not in a list of the names of variables  
#'
#' @param dsgroup dataframe
#' @param nameStrings A character vector of the names of the variables
#'    
#' @return dataframe in which the variables not in a list of the names of variables are deleted   
#'
#' @examples
#' \dontrun{
#' updateRDS(adj, c("subjectID", "adjDate", "adjCode"))
#' }
#'
#' @export
updateRDS <- function(dsgroup, nameStrings){
    nameV <- colnames(dsgroup)
    nameStrings2 <- nameStrings[nameStrings %in% nameV]
    dsgroup <- dsgroup[, nameStrings2, drop=FALSE]
}
