#' Save Dataset tsv file
#'
#' call write_tsv function and save the tsv file of the dataframe 
#'
#' @param idata dataframe
#' @param sdata A character string which is name of the dataframe
#' @param path A character string which is the path in which the tsv file of the dataframe is saved
#' @param header A logical value for the parameter col_names: default = TRUE
#'
#' @examples
#' \dontrun{
#' save2show(adj, "adj", "/home/name/workspace/Repository/___project/test/Datasource/csv1/adj")
#' }
#'
#' @export
save2show <- function(idata, sdata, path, header = TRUE){
  pdata1 = str_c(sdata,"tsv",sep = ".")
  pfile3 = str_c(path, pdata1, sep = .Platform$file.sep)
  write_tsv(idata, pfile3, append = FALSE, col_names = header)
}

