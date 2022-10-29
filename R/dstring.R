#' Obtain the actual variables  
#'
#' From a list of the names of the variables in a dataset,
#' this function returns the actual variables in the dataset. 
#'
#' @param lstrings A character vector of the names of variables
#'      In the nested function x is a parameter for a dataframe.
#'    
#' @return This function returns an R List whose objects are the columns in the dataframe;  
#'
#' @examples
#' \dontrun{
#' listV <- dstring(lstrings)
#' listVars <- listV(demo)
#' }
#'
dstring <- function(lstrings) {
  function(x) {
    ds0 <- list()
    for (i in 1:length(lstrings)){
        ds0[i] <- x[lstrings[i]]
    }
    return(ds0)
  }
}
