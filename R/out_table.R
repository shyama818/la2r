#' output the layered tables 
#'
#' This function splits the dataset for each realization of the layered variables and
#' output tables for the subsets. 
#'
#' @param dsgroup dataframe
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
#' out_table(demo, "demotable1", "BMI", c("Ethnic", "Sex"), c("AgeCat", "Treatment"), c("Elig"), 1, "meanSE", TRUE, 1, 2, 
#' "/home/shinichi/workspace/Repository/___project/test/Tree/testTree/BMI/Treatment/Sex/Country/table/___show",
#' "/home/shinichi/workspace/Repository/___project/test/Tree/testTree/BMI/Treatment/Sex/Country/table/___info/___levels")
#' }
#'
#' @export
out_table <- function(dsgroup, tableName, mainString, stratStrings, colStrings, rowStrings, type, statString, narmString, digitString, digitString2, path, levelpath){

pdata02 = str_c("*","tsv",sep = ".")
pfile021 = str_c(path, pdata02, sep = .Platform$file.sep)
unlink(pfile021)
pdata03 = str_c("*","tsv",sep = ".")
pfile031 = str_c(levelpath, pdata03, sep = .Platform$file.sep)
unlink(pfile031)

# Step 1: statString
listStrings <- c(stratStrings, colStrings, rowStrings)
outputM <- data_mp(dsgroup, listStrings, mainString, type, statString, narmString, digitString, digitString2)

# Step 2

if (length(stratStrings) == 0){
    NoRecord <- c("No Stratification Variables")
    dsh1 <- data.frame(NoRecord,stringsAsFactors=FALSE)
    save2show(dsh1,tableName,path)

    table_header(outputM, "All", colStrings, rowStrings, path)

} else {

stratV <- dstring(stratStrings)

stratVars <- stratV(outputM)
ap1 <- split(outputM, stratVars, drop = TRUE, sep = ";,;", lex.order = TRUE)
bp1 <- attributes(ap1)
outmat <- matrix(nrow=length(ap1), ncol=length(stratStrings))

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

    table_header(ap1[[gi0]], filenums, colStrings, rowStrings, path)
    if (gi0 == 1){
         table_header(ap1[[gi0]], "All", colStrings, rowStrings, path)
    }
 }  

# make data.frame
ingroup <- outputM[,stratStrings, drop = FALSE]
output2 <- simple_mat(ingroup, outmat)
save2show(output2,tableName,path)

}
}

