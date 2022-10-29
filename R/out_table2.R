#' output the layered tables 
#'
#' This function splits the dataset for each realization of the layered variables and
#' output tables with the total row and column for the subsets. 
#'
#' @param dsgroup dataframe
#' @param dsgroupC dataframe for the column total
#' @param dsgroupR dataframe for the row total
#' @param dsgroupT dataframe for the total
#' @param tableName A character string for the name of the file which contains all realization of the layered variables
#' @param mainString A character string for the name of the variable for each cell in the tables
#' @param stratStrings A character vector eash of which is name of the layered variables
#' @param colStrings A character vector eash of which is name of the column variables
#' @param rowStrings A character vector eash of which is name of the row variables
#' @param type a numeric string: 1 Type 1 for Base dataset, 2 Type 2 for the output dataset of the Stats or the ML node
#' @param statString a numeric string: choose from (N, sum, mean, median, min, max, mode, sd, se, meanSD, meanSE)
#' @param narmString a character string: TRUE or FALSE for na.rm parameter
#' @param digitString a numeric string: the minimum number of digits to the right of the decimal point
#' @param digitString2 a numeric string: the minimum number of digits to the right of the decimal point for SD or SE in meanSD and meanSE
#' @param path A character string which is the path of the directory in which tables for each realization of the layered variables are output.
#' @param levelpath A character string which is the path of the directory in which levels of each layered variables are output.
#' 
#' @examples
#' \dontrun{
#' out_table2(demoM, demoC, demoR, demoT, "demotable1", "BMI", c("Ethnic", "Sex"), c("Agecat", "Treatment"), c("Elig"), 1, "meanSE", TRUE, 1, 2, 
#' "/home/shinichi/workspace/Repository/___project/test/Tree/testTree/BMI/TRTGRP/SEX/COUNTRY/table/___show",
#' "/home/shinichi/workspace/Repository/___project/test/Tree/testTree/BMI/TRTGRP/SEX/COUNTRY/table/___info/___levels")
#' }d
#'
#' @export
out_table2 <- function(dsgroup, dsgroupC, dsgroupR, dsgroupT, tableName, mainString, stratStrings, colStrings, rowStrings, type, statString, narmString, digitString, digitString2, path, levelpath){

pdata02 = str_c("*","tsv",sep = ".")
pfile021 = str_c(path, pdata02, sep = .Platform$file.sep)
unlink(pfile021)
pdata03 = str_c("*","tsv",sep = ".")
pfile031 = str_c(levelpath, pdata03, sep = .Platform$file.sep)
unlink(pfile031)

# Step 1: statString
listStrings <- c(stratStrings, colStrings, rowStrings)
outputM <- data_mp(dsgroup, listStrings, mainString, type, statString, narmString, digitString, digitString2)

listStrings <- c(stratStrings, colStrings)
if (is.null(dsgroupC)){
outputC <- data_mp(dsgroup, listStrings, mainString, type, statString, narmString, digitString, digitString2)
} else {
outputC <- data_mp(dsgroupC, listStrings, mainString, type, statString, narmString, digitString, digitString2)
}

listStrings <- c(stratStrings, rowStrings)
if (is.null(dsgroupR)){
outputR <- data_mp(dsgroup, listStrings, mainString, type, statString, narmString, digitString, digitString2)
} else {
outputR <- data_mp(dsgroupR, listStrings, mainString, type, statString, narmString, digitString, digitString2)
}

listStrings <- stratStrings
if (is.null(dsgroupT)){
outputT <- data_mp(dsgroup, listStrings, mainString, type, statString, narmString, digitString, digitString2)
} else {
outputT <- data_mp(dsgroupT, listStrings, mainString, type, statString, narmString, digitString, digitString2)
}

# Step 2

if (length(stratStrings) == 0){
    NoRecord <- c("No Stratification Variables")
    dsh1 <- data.frame(NoRecord,stringsAsFactors=FALSE)
    save2show(dsh1,tableName,path)

    table_header2(outputM, outputC, outputR, outputT, "All", colStrings, rowStrings, path)

} else {

stratV <- dstring(stratStrings)

stratVars <- stratV(outputM)
ap1 <- split(outputM, stratVars, drop = TRUE, sep = ";,;", lex.order = TRUE)
bp1 <- attributes(ap1)
outmat <- matrix(nrow=length(ap1), ncol=length(stratStrings))

stratVarsC <- stratV(outputC)
ap1C <- split(outputC, stratVarsC, drop = TRUE, sep = ";,;", lex.order = TRUE)
bp1C <- attributes(ap1C)

stratVarsR <- stratV(outputR)
ap1R <- split(outputR, stratVarsR, drop = TRUE, sep = ";,;", lex.order = TRUE)
bp1R <- attributes(ap1R)

stratVarsT <- stratV(outputT)
ap1T <- split(outputT, stratVarsT, drop = TRUE, sep = ";,;", lex.order = TRUE)
bp1T <- attributes(ap1T)

levelfile(stratStrings, stratVars, levelpath)

for(gi0 in 1:length(ap1)) {
    if (length(stratStrings) == 1) {
            outmat[gi0,] <- bp1$names[gi0]
    } else {
            outmat[gi0,] <- mystrsplit(bp1$names[gi0], ";,;")
    }

    filenums <- ""
        for(k in 1:length(stratStrings)) {
            levelk <- levels(stratVars[[k]])
            indexk <- match(outmat[gi0,k],levelk)
            filenums <- str_c(filenums, indexk ,sep = "_")
        }

    dp1C <- NULL
    for (i0 in 1:length(ap1C)){
        cp1C <- mystrsplit(bp1C$names[i0], ";,;")
        if (all(cp1C == outmat[gi0,])) {
            dp1C <- ap1C[[i0]]
            break
        }
    }

    dp1R <- NULL
    for (i0 in 1:length(ap1R)){
        cp1R <- mystrsplit(bp1R$names[i0], ";,;")
        if (all(cp1R == outmat[gi0,])) {
            dp1R <- ap1R[[i0]]
            break
        }
    }

    dp1T <- NULL
    for (i0 in 1:length(ap1T)){
        cp1T <- mystrsplit(bp1T$names[i0], ";,;")
        if (all(cp1T == outmat[gi0,])) {
            dp1T <- ap1T[[i0]]
            break
        }
    }

    table_header2(ap1[[gi0]], dp1C, dp1R, dp1T, filenums, colStrings, rowStrings, path)
    if (gi0 == 1){
         table_header2(ap1[[gi0]], dp1C, dp1R, dp1T, "All", colStrings, rowStrings, path)
    }
   
 }  

# make data.frame
ingroup <- outputM[,stratStrings, drop = FALSE]
output2 <- simple_mat(ingroup, outmat)
save2show(output2,tableName,path)

}
}

