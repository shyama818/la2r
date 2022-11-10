#' output the layered lists 
#'
#' This function splits the dataset for each realization of the layered variables,  
#' we output a list for each subset.
#'
#' @param dsgroup dataframe
#' @param listName A character string for the name of the file which contains all realization of the layered variables
#' @param stratStrings A character vector eash of which is name of the layered variables
#' @param listStrings A character vector eash of which is name of the input variables in the list
#' @param path A character string which is the path of the directory in which lists for each realization of the layered variables are output.
#' @param levelpath A character string which is the path of the directory in which levels of each layered variables are output.
#'
#' @examples
#' \dontrun{
#' out_list(demo, "demolist1", c("Treatment", "Sex"), c("AgeCat", "Ethnic", "BMI", "Weight", "Height"), 2, 
#' "/home/shinichi/workspace/Repository/___project/test/Tree/testTree/BMI/Treatment/Sex/Country/anova/anova_type3/table011/___show",
#' "/home/shinichi/workspace/Repository/___project/test/Tree/testTree/BMI/Treatment/Sex/Country/anova/anova_type3/table011/___info/___levels")
#' }
#'
#' @export
out_list <- function(dsgroup, listName, stratStrings, listStrings, path, levelpath){

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

    dsout1 <- dsgroup[,listStrings, drop = FALSE]
    fileName <- str_c("l", "All")
    save2show(dsout1,fileName,path)

} else {
    stratV <- dstring(stratStrings)
    stratVars <- stratV(dsgroup)
    levelfile(stratStrings, stratVars, levelpath)

    subsetStrings <- c(stratStrings, listStrings)
    dsout1 <- dsgroup[,subsetStrings]

    ap0 <- split(dsout1, stratVars, drop = TRUE, sep = ";,;", lex.order = TRUE)
    bp0 <- attributes(ap0)
    outmat <- matrix(nrow=length(ap0),ncol=length(stratStrings))
    for(gi0 in 1:length(ap0)) {  
        if (length(stratStrings) == 1) {
            outmat[gi0,] <- bp0$names[gi0]
        } else {
            outmat[gi0,] <- mystrsplit(bp0$names[gi0], ";,;")
        }
        filenums <- ""
        for(k in 1:length(stratStrings)) {
            levelk <- levels(stratVars[[k]])
            indexk <- match(outmat[gi0,k],levelk)
            filenums <- str_c(filenums, indexk ,sep = "_")
        }
        fileName <- str_c("l", filenums)
        ap2 <- ap0[[gi0]][,listStrings]
        save2show(ap2,fileName,path)
        if (gi0 == 1){
            fileName <- str_c("l", "All")
            ap2 <- ap0[[gi0]][,listStrings]
            save2show(ap2,fileName,path)
        }
    }
    ingroup <- dsgroup[,stratStrings, drop = FALSE]
    output2 <- simple_mat(ingroup, outmat)
    save2show(output2,listName,path)
}
}

