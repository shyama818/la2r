#' output the layered figures 
#'
#' This function splits the dataset for each realization of the layered variables,  
#' we immplement a function given by .fun for each subset and output a figure.
#'
#' @param dsgroup dataframe
#' @param listName A character string for the name of the file which contains all realization of the layered variables
#' @param stratStrings A character vector eash of which is name of the layered variables
#' @param checkStrings A character vector eash of which is name of the check variables
#' @param type a numeric string: 1 Type 1 for Base dataset, 2 Type 2 for the output dataset of the Stats or the ML node
#' @param path A character string which is the path of the directory in which figures for each realization of the layered variables are output.
#' @param levelpath A character string which is the path of the directory in which levels of each layered variables are output.
#' @param .fun A function which is immplemented for the subset for the each realization of the layered variables
#' @param ... other arguments passed on to .fun
#'
#' @examples
#' \dontrun{
#' out_figure(demo, "demolist1", c("Ethnic", "Sex"), c("Agecat", "Treatment"), 1, 
#' "/home/shinichi/workspace/Repository/___project/test/Tree/testTree/BMI/TRTGRP/SEX/COUNTRY/barplot/___show",
#' "/home/shinichi/workspace/Repository/___project/test/Tree/testTree/BMI/TRTGRP/SEX/COUNTRY/barplot/___info/___levels", .fun = function(xx, yy) {
#' ggplot0 <- ggplot(data=xx, aes(x=Agecat, y=stat(count), fill=Treatment)) + 
#' geom_bar(stat="count", position=position_stack()) + 
#' labs(title ="Bar Plot of stat(count) based on Agecat", subtitle = yy) + 
#' theme(plot.title = element_text(hjust = 0.5)) + 
#' theme(plot.subtitle = element_text(hjust = 0.5))
#' return(ggplot0)
#' })
#' }
#'
#' @export
out_figure <- function(dsgroup, listName, stratStrings, checkStrings, type, path, levelpath, .fun = NULL, ...){

# clear folder
pdata01 = str_c("*","png",sep = ".")
pfile011 = str_c(path, pdata01, sep = .Platform$file.sep)
unlink(pfile011)
pdata02 = str_c("*","tsv",sep = ".")
pfile021 = str_c(path, pdata02, sep = .Platform$file.sep)
unlink(pfile021)
pdata03 = str_c("*","tsv",sep = ".")
pfile031 = str_c(levelpath, pdata03, sep = .Platform$file.sep)
unlink(pfile031)

if (length(stratStrings) == 0){
    NoRecord <- c("No Stratification Variables")
    dsh1 <- data.frame(NoRecord,stringsAsFactors=FALSE)
    save2show(dsh1,listName,path)

    fileName <- str_c("l", "All")
    pdata1 = str_c(fileName,"png",sep = ".")
    pfile3 = str_c(path, pdata1, sep = .Platform$file.sep)
    res <- .fun(dsgroup, ...)
    ggsave(pfile3, plot = res)

} else {
    stratV <- dstring(stratStrings)
    stratVars <- stratV(dsgroup)
    levelfile(stratStrings, stratVars, levelpath)

    ap0 <- split(dsgroup, stratVars, drop = TRUE, sep = ";,;", lex.order = TRUE)
    bp0 <- attributes(ap0)
    outmat <- matrix(nrow=length(ap0),ncol=length(stratStrings))
    deleteIndex <- c()
    for(ci in 1:length(ap0)) { 
        if (length(stratStrings) == 1) {
            outmat[ci,] <- bp0$names[ci]
        } else {
            outmat[ci,] <- mystrsplit(bp0$names[ci], ";,;")
        }
        subtList <- ""
        filenums <- ""
        for(k in 1:length(stratStrings)) {
            addstr <- str_c(stratStrings[k], "=" , outmat[ci,k] ,sep = " ") 
            if (k > 1 && k%%3 == 1){
                subtList <- str_c(subtList, addstr ,sep = ", \n")
            } else {
                subtList <- str_c(subtList, addstr ,sep = ", ")
            }
            
            levelk <- levels(stratVars[[k]])
            indexk <- match(outmat[ci,k],levelk)
            filenums <- str_c(filenums, indexk ,sep = "_")
        }
        subtList <- str_sub(subtList, start = 3)
        fileName <- str_c("l", filenums)
        pdata1 = str_c(fileName,"png",sep = ".")
        pfile3 = str_c(path, pdata1, sep = .Platform$file.sep)
        ap2 <- ap0[[ci]]
        check <- TRUE
        if (!is.null(checkStrings)){
            if (type == 1) {
                limit0 <- 2
            } else {
                limit0 <- 1
            }
            if (nrow(na.omit(ap2[, checkStrings])) < limit0) {
                check <- FALSE
            }
        }
        if (check == TRUE){
            res <- .fun(ap2, subtList, ...)
            if (!is.null(res)){
                ggsave(pfile3, plot = res)
            } else {
                deleteIndex <- c(deleteIndex, -ci)
            }
        } else {
            deleteIndex <- c(deleteIndex, -ci)
        } 
    }
    if (length(deleteIndex) > 0) {
        outmat2 <- matrix(outmat[deleteIndex, ], ncol=length(stratStrings))
    } else {
        outmat2 <- outmat
    }
    ingroup <- dsgroup[,stratStrings, drop = FALSE]
    output2 <- simple_mat(ingroup, outmat2)
    save2show(output2,listName,path)
}
}

