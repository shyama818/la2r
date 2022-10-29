#' Save Dataset Info
#'
#' call contents function and save the dataframe information and the level files for the factor variables
#'
#' @param idata dataframe
#' @param sdata A character string which is name of the dataframe
#' @param path A character string which is the path in which the dataframe info is saved
#'
#' @examples
#' \dontrun{
#' save2info(adj, "adj", "/home/name/workspace/Repository/___project/test/Datasource/csv1/adj")
#' }
#'
#' @export
save2info <- function(idata,sdata,path){
  k <- contents(idata, prlevels=FALSE, maxlevels=12)
  l <- k$contents
  l$names <- rownames(l)

  length0 <- nrow(l)
  f1 <- rownames(l)
  isfactor <- c(rep("",length0))
  isordered <- c(rep("",length0))
  for (i in 1:length0) {
      isfactor[i] <- is.factor(idata[[f1[i]]])
      isordered[i] <- is.ordered(idata[[f1[i]]])
  }
  l0 <- data.frame(names=f1,isfactor=isfactor,isordered=isordered,stringsAsFactors = FALSE)
  l2 <- left_join(l, l0, by = "names")

  k1 <- k$Levels
  length1 <- length(k1)
  if (length1>0){

    # delete level files
    pdata0021 = str_c("*","tsv",sep = ".")
    pfile0021 = str_c(path, "___levels", pdata0021, sep = .Platform$file.sep)
    unlink(pfile0021)

    v1 <- names(k1)
    # output Levels
    for (i in 1:length1) {
        k2 <- levels(idata[[v1[i]]])
        k1frame <- data.frame(k2)
        colnames(k1frame) <- v1[i]
        # pd01 <- str_c(sdata,names(k1[i]),sep = "_")
        pd02 <- str_c(names(k1[i]),"tsv",sep = ".")
        pfile0 = str_c(path, "___levels", pd02, sep = .Platform$file.sep)
        write_tsv(k1frame, pfile0, append = FALSE, col_names = FALSE)
    }
  } 

  row_n <- nrow(l2)
  y1 <- names(l2)
  # names, labels, storage mode, class, units, isfactor, isordered, number of factor levels, number of NAs, and NotNA
  m1 <- c("names", "Labels", "Storage", "Class", "Units", "isfactor", "isordered", "Levels", "NAs", "NotNA")
   
  for (i in 1:8) {
    y2 <- match(m1[i],y1)
    if (is.na(y2)) {
      y <- rep("",nrow(l2))
      l2 <- cbind(l2, y)
      col_n <- ncol(l2)
      colnames(l2)[col_n] <- m1[i]
    }
  }
  y2 <- match(m1[9],y1)
    if (is.na(y2)) {
      y <- colSums(is.na(idata))
      l2 <- cbind(l2, y)
      col_n <- ncol(l2)
      colnames(l2)[col_n] <- m1[9]
    }

  y0 <- rep(nrow(idata),nrow(l2)) 
  y <- y0 - l2[["NAs"]]
  l2 <- cbind(l2, y)
  col_n <- ncol(l2)
  colnames(l2)[col_n] <- m1[10]

  l3 <- l2
  metadata <- l3[,m1]
  
  pdata1 = str_c(sdata,"tsv",sep = ".")
  pfile1 = str_c(path, pdata1, sep = .Platform$file.sep)
  write_tsv(metadata, pfile1, append = FALSE, col_names = FALSE)

}

