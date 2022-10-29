#' output the headers for the table with the total row and column 
#'
#' This function output the headers for the table with the total row and column  
#'
#' @param dp1 dataframe
#' @param dp1C dataframe for the column total
#' @param dp1R dataframe for the row total
#' @param dp1T dataframe for the total
#' @param fName A character string for the name of the file
#' @param colStrings A character vector eash of which is name of the column variables
#' @param rowStrings A character vector eash of which is name of the row variables
#' @param path A character string which is the path of the directory in which the headers for the table are output.
#' 
#' @examples
#' \dontrun{
#' table_header2(dp0M, dp0C, dp0R, dp0T, "All", c("Agecat", "Treatment"), c("Elig"),
#' "/home/shinichi/workspace/Repository/___project/test/Tree/testTree/BMI/TRTGRP/SEX/COUNTRY/table/___show")
#' }
#'
table_header2 <- function(dp1, dp1C, dp1R, dp1T, fName, colStrings, rowStrings, path){

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

    if (!is.null(dp1C)) {
        col.varC <- vector(length=nrow(dp1C))
        for (l0 in 1:nrow(dp1C)){
        for (k0 in 1:nrow(colframe)){
            if (ncol(colframe) == 1){
                if (dp1C[colStrings[1]][l0,1] == colframe[k0,1]){
                    col.varC[l0] <- k0
                }
            } else if (ncol(colframe) == 2){
                if (dp1C[colStrings[1]][l0,1] == colframe[k0,1] & dp1C[colStrings[2]][l0,1] == colframe[k0,2]){
                    col.varC[l0] <- k0
                }
            } else if (ncol(colframe) == 3){
                if (dp1C[colStrings[1]][l0,1] == colframe[k0,1] & dp1C[colStrings[2]][l0,1] == colframe[k0,2] & dp1C[colStrings[3]][l0,1] == colframe[k0,3]){
                    col.varC[l0] <- k0
                }
            } 
        }
        } 
        dp1C$col.varC <- col.varC
    }

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

    if (!is.null(dp1R)) {
        row.varR <- vector(length=nrow(dp1R))
        for (l0 in 1:nrow(dp1R)){
        for (k0 in 1:nrow(rowframe)){
            if (ncol(rowframe) == 1){
                if (dp1R[rowStrings[1]][l0,1] == rowframe[k0,1]){
                    row.varR[l0] <- k0
                }
            } else if (ncol(rowframe) == 2){
                if (dp1R[rowStrings[1]][l0,1] == rowframe[k0,1] & dp1R[rowStrings[2]][l0,1] == rowframe[k0,2]){
                    row.varR[l0] <- k0
                }
            } 
        }
        } 
        dp1R$row.varR <- row.varR
    }

    n_row1 <- nrow(rowframe)+1
    n_col1 <- nrow(colframe)+1
    outmatM <- matrix("", nrow=n_row1,ncol=n_col1)
    for (l0 in 1:nrow(dp1)){
    for (c0 in 1:nrow(colframe)){
    for (r0 in 1:nrow(rowframe)){   
        if (dp1$col.var[l0] == c0 &  dp1$row.var[l0] == r0) outmatM[r0,c0] <- dp1$main.var[l0]
    }
    }
    }

    if (!is.null(dp1C)) {
        for (l0 in 1:nrow(dp1C)){
        for (c0 in 1:nrow(colframe)){   
            if (dp1C$col.varC[l0] == c0) outmatM[n_row1,c0] <- dp1C$main.var[l0]
        }
        }
    }
    
    if (!is.null(dp1R)) {
        for (l0 in 1:nrow(dp1R)){
        for (r0 in 1:nrow(rowframe)){   
            if (dp1R$row.varR[l0] == r0) outmatM[r0,n_col1] <- dp1R$main.var[l0]
        }
        }
    }
    
    if (!is.null(dp1T)) {
        outmatM[n_row1,n_col1] <- dp1T$main.var[1]
    }

    cn <- ncol(colframe)
    rn <- nrow(colframe)
    col_name = list()
    for (i in 1:cn){
        col_name[[i]] <- str_c(colframe[,i], "(", colStrings[i], ")")
    }
    colheader <- col_name[[cn]]
    colheader[rn+1] <- "Total"

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
        j1 <- j1+1
        colgheader[j1] <- " "
        k1 <- k1+1
        colgheader[j1] <- str_c(colgheader[j1], k1, sep=";,;")

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
        j1 <- j1+1
        colggheader[j1] <- " "
        k1 <- k1+1
        colggheader[j1] <- str_c(colggheader[j1], k1, sep=";,;")

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
    rowheader[rn+1] <- "Total"

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
        j1 <- j1+1
        rowgheader[j1] <- " "
        k1 <- k1+1
        rowgheader[j1] <- str_c(rowgheader[j1], k1, sep=";,;")

        fileName <- str_c("l", fName, "_rowgheader")
        orowgheader <- as.data.frame(rowgheader)
        save2show(orowgheader,fileName,path,header = FALSE)
    }

    outdata <- as.data.frame(outmatM)
    colnames(outdata) <- colheader
    fileName <- str_c("l", fName)
    save2show(outdata,fileName,path)

}

