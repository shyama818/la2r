#' Save Dataset
#'
#' call save2show, save2info, save2rds functions
#'
#' @param idata dataframe
#' @param sdata A character string which is name of the dataframe
#' @param path A character string which is the path of the root directory under which the dataframe is saved
#'
#' @examples
#' \dontrun{
#' save2data(adj, "adj", "/home/name/workspace/Repository/___project/test/Datasource/csv1/adj")
#' }
#'
#' @export
save2data <- function(idata, sdata, path){  
    path1 = str_c(path, "___show", sep = .Platform$file.sep)
    save2show(idata, sdata, path1)

    path2 = str_c(path, "___info", sep = .Platform$file.sep)
    save2info(idata, sdata, path2)

    path3 = str_c(path, "___data", sep = .Platform$file.sep)
    save2rds(idata, sdata, path3)
}
