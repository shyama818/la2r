#' Save R Dataset
#'
#' call saveRDS function and save the dataframe 
#'
#' @param idata dataframe
#' @param sdata A character string which is name of the dataframe
#' @param path A character string which is the path in which the dataframe is saved
#'
#' @examples
#' \dontrun{
#' save2rds(adj, "adj", "/home/name/workspace/Repository/___project/test/Datasource/csv1/adj")
#' }
#'
#' @export
save2rds <- function(idata,sdata,path){
  pdata2 = str_c(sdata,"rds",sep = ".")
  pfile2 = str_c(path,pdata2, sep = .Platform$file.sep)
  saveRDS(idata, file = pfile2)
}

