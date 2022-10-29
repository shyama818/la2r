#' wrap strsplit function
#'
#' wrap strsplit function
#' In strsplit function if the string after the last delimiter string is "",
#' the character array does not include the last string.
#' This function returns the character array whose the last entry is "". 
#'
#' @param string a character string: 
#' @param dlm a character string: delimiter string
#'
#' @return a character vector
#'
#' @examples
#' \dontrun{
#' mystrsplit("There exist multiple records for each realisation of the variables.", " ")
#' }
#'
mystrsplit <- function(string, dlm){
spl <- strsplit(string, dlm)[[1]]
len1 <- str_length(string)
len0 <- len1 - (str_length(dlm)-1)
if (substr(string,len0,len1) == dlm){
    spl <- c(strsplit(string, dlm)[[1]], "") 
} 
return(spl)
}

