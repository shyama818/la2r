#' output the headers for the table 
#'
#' This function output the headers for the table   
#'
#' @param dp1 dataframe
#' @param fName A character string for the name of the file
#' @param colStrings A character vector eash of which is name of the column variables
#' @param rowStrings A character vector eash of which is name of the row variables
#' @param path A character string which is the path of the directory in which the headers for the table are output.
#' 
#' @examples
#' \dontrun{
#' table_header(dp0, "All", c("Agecat", "Treatment"), c("Elig"),
#' "/home/shinichi/workspace/Repository/___project/test/Tree/testTree/BMI/TRTGRP/SEX/COUNTRY/table/___show")
#' }
#'
table_header <- function(dp1, fName, colStrings, rowStrings, path){

    colframe <- table_strat_s1(dp1, colStrings)
    col.var <- vector(length=nrow(dp1))
    for (l0 in 1:nrow(dp1)){
    for (k0 in 1:nrow(colframe)){
        if (ncol(colframe) == 1){
            if (dp1[colStrings[1]][l0,1] == colframe[k0,1]){
                col.var[l0] <- k0
            }
        } else if (ncol(colframe) == 2){
            if (dp1[colStrings[1]][l0,1] == colframe[k0,1] & dp1[colStrings[2]][l0,1] == colframe[k0,2]){
                col.var[l0] <- k0
            }
        } else if (ncol(colframe) == 3){
            if (dp1[colStrings[1]][l0,1] == colframe[k0,1] & dp1[colStrings[2]][l0,1] == colframe[k0,2] & dp1[colStrings[3]][l0,1] == colframe[k0,3]){
                col.var[l0] <- k0
            }
        } 
    }
    } 
    dp1$col.var <- col.var

    rowframe <- table_strat_s1(dp1, rowStrings)
    row.var <- vector(length=nrow(dp1))
    for (l0 in 1:nrow(dp1)){
    for (k0 in 1:nrow(rowframe)){
        if (ncol(rowframe) == 1){
            if (dp1[rowStrings[1]][l0,1] == rowframe[k0,1]){
                row.var[l0] <- k0
            }
        } else if (ncol(rowframe) == 2){
            if (dp1[rowStrings[1]][l0,1] == rowframe[k0,1] & dp1[rowStrings[2]][l0,1] == rowframe[k0,2]){
                row.var[l0] <- k0
            }
        } 
    }
    } 
    dp1$row.var <- row.var

    outputM <- matrix("", nrow=nrow(rowframe),ncol=nrow(colframe))
    for (l0 in 1:nrow(dp1)){
    for (c0 in 1:nrow(colframe)){
    for (r0 in 1:nrow(rowframe)){   
        if (dp1$col.var[l0] == c0 &  dp1$row.var[l0] == r0) outputM[r0,c0] <- dp1$main.var[l0]
    }
    }
    }

    cn <- ncol(colframe)
    rn <- nrow(colframe)
    col_name = list()
    for (i in 1:cn){
        col_name[[i]] <- str_c(colframe[,i], "(", colStrings[i], ")")
    }
    colheader <- col_name[[cn]]
    colgheader <- list()
    colggheader <- list()
    if (cn >= 2) {
        cn2 <- cn-1
        previous = ""
        j1 <- 0
        for (i1 in 1:rn){
            k1 <- i1-1
            if (!strcmp(previous, col_name[[cn2]][i1])){
                j1 <- j1+1
                colgheader[j1] <- str_c(col_name[[cn2]][i1])
            }
            colgheader[j1] <- str_c(colgheader[j1], k1, sep=";,;")
            previous = col_name[[cn2]][i1]
        }
        fileName <- str_c("l", fName, "_gheader")
        outgheader <- as.data.frame(colgheader)
        save2show(outgheader,fileName,path,header = FALSE)
    }
    if (cn == 3) {
        cn2 <- cn-2
        previous = ""
        j1 <- 0
        for (i1 in 1:rn){
            k1 <- i1-1
            if (!strcmp(previous, col_name[[cn2]][i1])){
                j1 <- j1+1
                colggheader[j1] <- str_c(col_name[[cn2]][i1])
            }
            colggheader[j1] <- str_c(colggheader[j1], k1, sep=";,;")
            previous = col_name[[cn2]][i1]
        }
        fileName <- str_c("l", fName, "_ggheader")
        outggheader <- as.data.frame(colggheader)
        save2show(outggheader,fileName,path,header = FALSE)
    }
    
    cn <- ncol(rowframe)
    rn <- nrow(rowframe)
    row_name = list()
    for (i in 1:cn){
        row_name[[i]] <- str_c(rowframe[,i], "(", rowStrings[i], ")")
    }
    rowheader <- row_name[[cn]]
    fileName <- str_c("l", fName, "_rowheader")
    orowheader <- as.data.frame(rowheader)
    save2show(orowheader,fileName,path,header = FALSE)

    rowgheader <- list()
    if (cn >= 2) {
        cn2 <- cn-1
        previous = ""
        j1 <- 0
        for (i1 in 1:rn){
            k1 <- i1-1
            if (!strcmp(previous, row_name[[cn2]][i1])){
                j1 <- j1+1
                rowgheader[j1] <- str_c(row_name[[cn2]][i1])
            }
            rowgheader[j1] <- str_c(rowgheader[j1], k1, sep=";,;")
            previous = row_name[[cn2]][i1]
        }
        fileName <- str_c("l", fName, "_rowgheader")
        orowgheader <- as.data.frame(rowgheader)
        save2show(orowgheader,fileName,path,header = FALSE)
    }

    outdata <- as.data.frame(outputM)
    colnames(outdata) <- colheader
    fileName <- str_c("l", fName)
    save2show(outdata,fileName,path)

}

