#' Obtain the actual variables  
#'
#' From a list of the names of the variables in a dataset,
#' this function returns the actual variables in the dataset. 
#'
#' @param lstrings A character vector of the names of variables
#' @param x dataframe
#'    
#' @return This function returns a list whose objects are the variables in the dataset;  
#'
#' @examples
#' listV <- dstring(lstrings)
#' listVars <- listV(x)
#'
dstring <- function(lstrings) {
  function(x) {
    ds0 <- list()
    for (i in 1:length(lstrings)){
        ds0[i] <- x[lstrings[i]]
    }
    return(ds0)
  }
}

#' Update a dataframe
#'
#' Update a dataframe by deleting the variables which are not in a list of the names of variables  
#'
#' @param dsgroup dataframe
#' @param nameStrings A character vector of the names of the variables
#'    
#' @return dataframe in which the variables not in a list of the names of variables are deleted   
#'
#' @examples
#' updateRDS(adj, c("subjectID", "adjDate", "adjCode"))
#'
updateRDS <- function(dsgroup, nameStrings){
    nameV <- colnames(dsgroup)
    nameStrings2 <- nameStrings[nameStrings %in% nameV]
    dsgroup <- dsgroup[, nameStrings2, drop=FALSE]
}

#' Normalize the numeric variable
#'
#' This function normalizes the numeric variable by computing: 
#' the normalized variable = the numeric variable (lhs) - the mean of the numeric variable (lhs) for each value of the group variable (rhs)
#'
#' @param dsgroup dataframe
#' @param lhs A character string which is name of the main variable of numeric type
#' @param rhs A character string which is name of the group variable of factor type
#'    
#' @return the normalized variable
#'
#' @examples
#' datacen(x, "BMI", "SEX")
#'
datacen <- function(dsgroup, lhs, rhs){
    datac <- ddply(dsgroup, c(rhs), 
        .fun = function(xxx) { 
        summary0(xxx, lhs, TRUE) 
    })
    means0 <- datac[,"mean"]
    meancen <- dsgroup[,lhs]-means0[as.numeric(dsgroup[,rhs])]
    return(meancen)
}

#' Check the uniqueness for the subset for each realization of the variables
#'
#' This function check that there is only a single record for the subset corresponding to each relization of the variables
#'
#' @param x dataframe
#' @param listStrings A character vector which are names of variables
#' @param type A numeric string: 1 error (stop the transaction), 2 warning (warning a message)
#' @param text A character string which is a message printed out
#'    
#'
#' @examples
#' checkUnique(x, c("BMI", "SEX"), 1, "There exist multiple records for each realisation of the variables.")
#'
checkUnique <- function(x, listStrings, type, text) {
    if (length(listStrings) > 0){
    listV <- dstring(listStrings)
    listVars <- listV(x)
    ap0 <- split(x, listVars, drop = TRUE, sep = ";,;", lex.order = TRUE)
    for(gi0 in 1:length(ap0)) {
        spl <- ap0[[gi0]]    
        if (nrow(spl) > 1){
            if (type==1){
                stop(text)
            } else if (type==2){
                warning(text)
            } 
        }
    }
    }
}

#' Save Dataset
#'
#' call save_show, save_info, save_rds functions
#'
#' @param idata dataframe
#' @param sdata A character string which is name of the dataframe
#' @param path A character string which is the path of the root directory under which the dataframe is saved
#'
#' @examples
#' save_data(adj, "adj", "/home/name/workspace/Repository/___project/test/Datasource/csv1/adj")
#'
#' @export
save_data <- function(idata, sdata, path){  
    path1 = str_c(path, "___show", sep = .Platform$file.sep)
    save_show(idata, sdata, path1)

    path2 = str_c(path, "___info", sep = .Platform$file.sep)
    save_info(idata, sdata, path2)

    path3 = str_c(path, "___data", sep = .Platform$file.sep)
    save_rds(idata, sdata, path3)
}

#' convert a matrix which is the first object of a list to a dataframe
#'
#' convert a matrix which is the first object of a list to a dataframe
#'
#' @param idata list
#'
#' @return dataframe
#'
#' @examples
#' mat2data(adj)
#'
mat2data <- function(idata){
  if (is.data.frame(idata) == FALSE && is.list(idata) == TRUE){
    imat <- idata[[1]]
    if (is.matrix(imat) == TRUE){
        col_name <- colnames(imat)
        if (is.null(col_name)){
            coln <- ncol(imat)
            colnames(imat) <- paste0("col",as.character(1:coln))
        }
    }
    idata <- as.data.frame(imat)
  }
  return(idata)
}

#' Save Dataset Info
#'
#' call contents function and save the dataframe information and the level files for the factor variables
#'
#' @param idata dataframe
#' @param sdata A character string which is name of the dataframe
#' @param path A character string which is the path in which the dataframe info is saved
#'
#' @examples
#' save_info(adj, "adj", "/home/name/workspace/Repository/___project/test/Datasource/csv1/adj")
#'
#' @export
save_info <- function(idata,sdata,path){
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

#' Save R Dataset
#'
#' call saveRDS function and save the dataframe 
#'
#' @param idata dataframe
#' @param sdata A character string which is name of the dataframe
#' @param path A character string which is the path in which the dataframe is saved
#'
#' @examples
#' save_rds(adj, "adj", "/home/name/workspace/Repository/___project/test/Datasource/csv1/adj")
#'
#' @export
save_rds <- function(idata,sdata,path){
  pdata2 = str_c(sdata,"rds",sep = ".")
  pfile2 = str_c(path,pdata2, sep = .Platform$file.sep)
  saveRDS(idata, file = pfile2)
}

#' Save Dataset tsv file
#'
#' call write_tsv function and save the tsv file of the dataframe 
#'
#' @param idata dataframe
#' @param sdata A character string which is name of the dataframe
#' @param path A character string which is the path in which the tsv file of the dataframe is saved
#'
#' @examples
#' save_show(adj, "adj", "/home/name/workspace/Repository/___project/test/Datasource/csv1/adj")
#'
#' @export
save_show <- function(idata, sdata, path, header = TRUE){
  pdata1 = str_c(sdata,"tsv",sep = ".")
  pfile3 = str_c(path, pdata1, sep = .Platform$file.sep)
  write_tsv(idata, pfile3, append = FALSE, col_names = header)
}

#' convert matrix to dataframe
#'
#' convert matrix x to dataframe  
#' If the name of each column of x is contained in the dataframe (indata), 
#' it is converted to the same type of the variable in indata.
#'
#' @param indata dataframe
#' @param inmat matrix
#'
#' @return dataframe
#'
#' @examples
#' simple_mat(adj, inmat)
#'
simple_mat <- function(indata, inmat){

col_name <- colnames(indata)
col_type <- rep(NA,length(col_name))
factor_type <- rep(NA,length(col_name))
double_type <- rep(NA,length(col_name))
integer_type <- rep(NA,length(col_name))
logical_type <- rep(NA,length(col_name))
character_type <- rep(NA,length(col_name))
complex_type <- rep(NA,length(col_name))
for (i in 1:length(col_name)) {
    factor_type[i] <- is.factor(unlist(indata[col_name[i]]))
    double_type[i] <- is.double(unlist(indata[col_name[i]]))
    integer_type[i] <- is.integer(unlist(indata[col_name[i]]))
    logical_type[i] <- is.logical(unlist(indata[col_name[i]]))
    character_type[i] <- is.character(unlist(indata[col_name[i]]))
    complex_type[i] <- is.complex(unlist(indata[col_name[i]]))
}

outdata <- NULL
for (i in 1:ncol(indata)) {
    if (i==1){
        if (factor_type[i] == TRUE){
            outdata <- data.frame(factor(inmat[,i], ordered = is.ordered(indata[col_name[i]][,1]), levels = levels(indata[col_name[i]][,1])))
        } else if (double_type[i] == TRUE){
            outdata <- data.frame(as.double(inmat[,i]))
        } else if (integer_type[i] == TRUE){
            outdata <- data.frame(as.integer(inmat[,i]))
        } else if (logical_type[i] == TRUE){
            outdata <- data.frame(as.logical(inmat[,i]))
        } else if (character_type[i] == TRUE){
            outdata <- data.frame(as.character(inmat[,i]))
        } else if (complex_type[i] == TRUE){
            outdata <- data.frame(as.complex(inmat[,i]))
        }
    } else {
        if (factor_type[i] == TRUE){
            outdata <- data.frame(outdata,factor(inmat[,i], ordered = is.ordered(indata[col_name[i]][,1]), levels = levels(indata[col_name[i]][,1])))
        } else if (double_type[i] == TRUE){
            outdata <- data.frame(outdata,as.double(inmat[,i]))
        } else if (integer_type[i] == TRUE){
            outdata <- data.frame(outdata,as.integer(inmat[,i]))
        } else if (logical_type[i] == TRUE){
            outdata <- data.frame(outdata,as.logical(inmat[,i]))
        } else if (character_type[i] == TRUE){
            outdata <- data.frame(outdata,as.character(inmat[,i]))
        } else if (complex_type[i] == TRUE){
            outdata <- data.frame(outdata,as.complex(inmat[,i]))
        }
    }   
}
colnames(outdata) <- col_name

return(outdata)

}

#' apply a summary statistic to a numeric vector and apply a format function to the result
#'
#' apply a summary statistic (N, sum, mean, median, min, max, mode, sd, se, meanSD, meanSE) to a numeric vector
#' and apply a round function and a format function to the result if type = 1.
#' check uniquness; then 1. apply a round and format function or 2. return the input value as it is if type = 2. 
#'
#' @param vpl a numeric vector
#' @param type a numeric string: 1 Type 1 for Base dataset, 2 Type 2 for the output dataset of Stats or ML node
#' @param statString a numeric string: choose from (N, sum, mean, median, min, max, mode, sd, se, meanSD, meanSE)
#' @param narmString a character string: TRUE or FALSE for na.rm parameter
#' @param digitString a numeric string: the minimum number of digits to the right of the decimal point
#' @param digitString2 a numeric string: the minimum number of digits to the right of the decimal point for SD or SE in meanSD and meanSE
#'
#' @return a character string: result
#'
#' @examples
#' stats_mp(vector0, 1, "meanSE", TRUE, 1, 2)
#'
stats_mp <- function(vpl, type, statString, narmString, digitString, digitString2){
    if (type==1){
        if (statString == "identity"){
            if (length(vpl) > 1){
                stop("There exist multiple records for each realisation of stratification, column and row variables.")
            } else {
                main0 <- format(round(vpl, as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString))
            }
        } else if (statString == "list"){
            main0 <- ""
            for (k0 in 1:length(vpl)){
                if (k0 == 1){
                    if (is.na(vpl[k0])){
                        main0 <- "NA"
                    } else {
                        main0 <- format(round(vpl[k0], as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString))
                    }  
                } else {
                    if (is.na(vpl[k0])){
                        main0 <- str_c(main0, "NA", sep = ", ")
                    } else {
                        main0 <- str_c(main0, format(round(vpl[k0], as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString)), sep = ", ")
                    }
                }   
            }
        } else if (statString == "N"){
            main0 <- length2(vpl, na.rm = narmString)
        } else if (statString == "sum"){
            main0 <- format(round(sum(vpl, na.rm = narmString), as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString))
        } else if (statString == "mean"){
            main0 <- format(round(mean(vpl, na.rm = narmString), as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString))
        } else if (statString == "median"){
            main0 <- format(round(median(vpl, na.rm = narmString), as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString))
        } else if (statString == "min"){
            main0 <- format(round(min(vpl, na.rm = narmString), as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString))
        } else if (statString == "max"){
            main0 <- format(round(max(vpl, na.rm = narmString), as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString))
        } else if (statString == "mode"){
            main0 <- mymode(vpl)
        } else if (statString == "sd"){
            main0 <- format(round(sd(vpl, na.rm = narmString), as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString))
        } else if (statString == "se"){
            main0 <- format(round(sd(vpl, na.rm = narmString)/sqrt(length2(vpl, na.rm=narmString)), as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString))
        } else if (statString == "meanSD"){
            if (length2(vpl, na.rm=narmString) > 1){
                main0 <- str_c(format(round(mean(vpl, na.rm = narmString), as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString)), format(round(sd(vpl, na.rm = narmString), as.integer(digitString2)), digits = as.integer(digitString2), nsmall = as.integer(digitString2)), sep = " +- ")
            } else {
                main0 <- format(round(mean(vpl, na.rm = narmString), as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString))
            }
        } else if (statString == "meanSE"){
            if (length2(vpl, na.rm=narmString) > 1){
                main0 <- str_c(format(round(mean(vpl, na.rm = narmString), as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString)), format(round(sd(vpl, na.rm = narmString)/sqrt(length2(vpl, na.rm=narmString)), as.integer(digitString2)), digits = as.integer(digitString2), nsmall = as.integer(digitString2)), sep = " +- ")
            } else {
                main0 <- format(round(mean(vpl, na.rm = narmString), as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString))
            }
        }  
    } else if (type==2) {
        if (length(vpl) > 1){
            stop("There exist multiple records for each realisation of stratification, column and row variables.")
        } else if (is.null(digitString)) {
            main0 <- vpl
        } else {
            main0 <- format(round(vpl, as.integer(digitString)), digits = as.integer(digitString), nsmall = as.integer(digitString))
        }
    }
    return(main0)
} 

#' wrap strsplit function
#'
#' wrap strsplit function
#' In strsplit function if the string after the last delimiter string is "",
#' the character array does not include the last string.
#' This function returns the character array whose the last entry is "". 
#'
#' @param string a character string: 
#' @param dlm a character string: delimiter string
#'
#' @return a character vector
#'
#' @examples
#' mystrsplit("There exist multiple records for each realisation of the variables.", " ")
#'
mystrsplit <- function(string, dlm){
spl <- strsplit(string, dlm)[[1]]
len1 <- str_length(string)
len0 <- len1 - (str_length(dlm)-1)
if (substr(string,len0,len1) == dlm){
    spl <- c(strsplit(string, dlm)[[1]], "") 
} 
return(spl)
}

#' Output the levels of the factor variables
#'
#' Output the levels of the factor variables in the directory of levelpath. 
#'
#' @param stratStrings A character vector of the names of variables
#' @param stratVars A list whose objects are the variables
#' @param levelpath A character string which is the path in which the levels of the factor variable is saved
#'
#' @examples
#' levelfile(lstrings, listVars, "/home/shinichi/workspace/Repository/___project/test/Tree/testTree/BMI/TRTGRP/SEX/anova/anova_type3/table11/___info/___levels")
#'
levelfile <- function(stratStrings, stratVars, levelpath){
    for(k in 1:length(stratStrings)) {
            levelk <- levels(stratVars[[k]])
            k1frame <- data.frame(levelk)
            colnames(k1frame) <- stratStrings[k]
            pd02 <- str_c(stratStrings[k],"tsv",sep = ".")
            pfile0 = str_c(levelpath, pd02, sep = .Platform$file.sep)
            write_tsv(k1frame, pfile0, append = FALSE, col_names = FALSE)
    }
}

#' For each realization of the variables (listStrings), call the stats_mp function for the variable (mainString)
#'
#' For each realization of the variables (listStrings),
#' apply the stats_mp function to the variable (mainString).
#'
#' @param dsgroup dataframe
#' @param listStrings A character vector of the names of variables
#' @param mainString a numeric string: the name of the variable
#' @param type a numeric string: 1 Type 1 for Base dataset, 2 Type 2 for the output dataset of Stats or ML node
#' @param statString a numeric string: choose from (N, sum, mean, median, min, max, mode, sd, se, meanSD, meanSE)
#' @param narmString a character string: TRUE or FALSE for na.rm parameter
#' @param digitString a numeric string: the minimum number of digits to the right of the decimal point
#' @param digitString2 a numeric string: the minimum number of digits to the right of the decimal point for SD or SE in meanSD and meanSE
#'
#' @return a character string
#'
#' @examples
#' data_mp(demo, c("Treatment", "Sex", "Ethnic"), BMI, 1, "meanSE", TRUE, 1, 2)
#'
data_mp <- function(dsgroup, listStrings, mainString, type, statString, narmString, digitString, digitString2){
if (is.null(listStrings)){
    spl <- dsgroup[, mainString]
    vpl <- as.vector(unlist(spl))   
    main1 <- stats_mp(vpl, type, statString, narmString, digitString, digitString2)
    output1 <- data.frame(main.var=main1, stringsAsFactors = FALSE)
} else {
    listV <- dstring(listStrings)
    listVars <- listV(dsgroup)
    ap0 <- split(dsgroup, listVars, drop = TRUE, sep = ";,;", lex.order = TRUE)
    bp0 <- attributes(ap0)
    outmat0 <- matrix(nrow=length(ap0),ncol=length(listStrings))
    main1 <- vector(length=length(ap0))
    for(gi0 in 1:length(ap0)) {
        if (length(listStrings) == 1) {
            outmat0[gi0,] <- bp0$names[gi0]
        } else {
            outmat0[gi0,] <- mystrsplit(bp0$names[gi0], ";,;")
        }
        spl <- ap0[[gi0]][, mainString]
        vpl <- as.vector(unlist(spl))   
        main1[gi0] <- stats_mp(vpl, type, statString, narmString, digitString, digitString2)
    }
ingroup <- dsgroup[,listStrings, drop = FALSE]
output0 <- simple_mat(ingroup, outmat0)
output1 <- data.frame(output0, main.var=main1, stringsAsFactors = FALSE)
}
return(output1)
}

#' output a dataframe which contains all realization of the variables
#'
#' This function generate a dataframe which contains all realization of the variables given by colStrings
#'
#' @param dsgroup dataframe
#' @param colStrings A character vector eash of which is name of the variables
#'
#' @return a dataframe which contains all realization of the variables
#'
#' @examples
#' table_strat_s1(demo, c("Ethnic", "Sex", "Agecat", "Treatment"))
#'
table_strat_s1 <- function(dsgroup, colStrings){

colV <- dstring(colStrings)
colVars <- colV(dsgroup)

ap1 <- split(dsgroup, colVars, drop = FALSE, sep = ";,;", lex.order = TRUE)
bp1 <- attributes(ap1)

outmat <- matrix(nrow=length(ap1),ncol=length(colStrings))
for(i0 in 1:length(ap1)) {
    if (length(colStrings) == 1) {
            outmat[i0,] <- bp1$names[i0]
    } else {
            outmat[i0,] <- mystrsplit(bp1$names[i0], ";,;")
    }
}

# make data.frame
ingroup <- dsgroup[,colStrings, drop = FALSE]
output2 <- simple_mat(ingroup, outmat)
return(output2)

}

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
#' out_table(demo, "demotable1", "BMI", c("Ethnic", "Sex"), c("Agecat", "Treatment"), c("Elig"), 1, "meanSE", TRUE, 1, 2, 
#' "/home/shinichi/workspace/Repository/___project/test/Tree/testTree/BMI/TRTGRP/SEX/COUNTRY/table/___show",
#' "/home/shinichi/workspace/Repository/___project/test/Tree/testTree/BMI/TRTGRP/SEX/COUNTRY/table/___info/___levels")
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
    save_show(dsh1,tableName,path)

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
save_show(output2,tableName,path)

}
}

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
#' table_header(dp0, "All", c("Agecat", "Treatment"), c("Elig"),
#' "/home/shinichi/workspace/Repository/___project/test/Tree/testTree/BMI/TRTGRP/SEX/COUNTRY/table/___show")
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
        save_show(outgheader,fileName,path,header = FALSE)
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
        save_show(outggheader,fileName,path,header = FALSE)
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
    save_show(orowheader,fileName,path,header = FALSE)

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
        save_show(orowgheader,fileName,path,header = FALSE)
    }

    outdata <- as.data.frame(outputM)
    colnames(outdata) <- colheader
    fileName <- str_c("l", fName)
    save_show(outdata,fileName,path)

}

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
#' out_table2(demoM, demoC, demoR, demoT, "demotable1", "BMI", c("Ethnic", "Sex"), c("Agecat", "Treatment"), c("Elig"), 1, "meanSE", TRUE, 1, 2, 
#' "/home/shinichi/workspace/Repository/___project/test/Tree/testTree/BMI/TRTGRP/SEX/COUNTRY/table/___show",
#' "/home/shinichi/workspace/Repository/___project/test/Tree/testTree/BMI/TRTGRP/SEX/COUNTRY/table/___info/___levels")
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
    save_show(dsh1,tableName,path)

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
save_show(output2,tableName,path)

}
}

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
#' table_header2(dp0M, dp0C, dp0R, dp0T, "All", c("Agecat", "Treatment"), c("Elig"),
#' "/home/shinichi/workspace/Repository/___project/test/Tree/testTree/BMI/TRTGRP/SEX/COUNTRY/table/___show")
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
        save_show(outgheader,fileName,path,header = FALSE)
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
        save_show(outggheader,fileName,path,header = FALSE)
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
    save_show(orowheader,fileName,path,header = FALSE)

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
        save_show(orowgheader,fileName,path,header = FALSE)
    }

    outdata <- as.data.frame(outmatM)
    colnames(outdata) <- colheader
    fileName <- str_c("l", fName)
    save_show(outdata,fileName,path)

}

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
#'
#' @examples
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
    save_show(dsh1,listName,path)

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
    save_show(output2,listName,path)
}
}

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
#' out_list(demo, "demolist1", c("Treatment", "Sex"), c("Agecat", "Ethnic", "BMI", "Weight", "Height"), 2, 
#' "/home/shinichi/workspace/Repository/___project/test/Tree/testTree/BMI/TRTGRP/SEX/COUNTRY/anova/anova_type3/table011/___show",
#' "/home/shinichi/workspace/Repository/___project/test/Tree/testTree/BMI/TRTGRP/SEX/COUNTRY/anova/anova_type3/table011/___info/___levels")
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
    save_show(dsh1,listName,path)

    dsout1 <- dsgroup[,listStrings, drop = FALSE]
    fileName <- str_c("l", "All")
    save_show(dsout1,fileName,path)

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
        save_show(ap2,fileName,path)
        if (gi0 == 1){
            fileName <- str_c("l", "All")
            ap2 <- ap0[[gi0]][,listStrings]
            save_show(ap2,fileName,path)
        }
    }
    ingroup <- dsgroup[,stratStrings, drop = FALSE]
    output2 <- simple_mat(ingroup, outmat)
    save_show(output2,listName,path)
}
}

#' A framework to conduct the layered analysis  
#'
#' This function provides a framework to conduct the layered analysis. 
#' For the subset for each realization of the layered variables,  
#' it immplements a function given by .fun and
#' output an R List which contains the output datasets. 
#'
#' @param dsgroup dataframe
#' @param stratStrings A character vector eash of which is name of the layered variables
#' @param outputStrings A character vector eash of which is name of the output datasets
#' @param .fun A function which is immplemented for the subset for the each realization of the layered variables
#'    
#' @return an R List which contains the output datasets.  
#'
#' @examples
#' ta_strat(demo, c("Treatment", "Sex"), c("Ethnic"), .fun = function(xx) {
#' anova1_wrap(xx, "BMI","Agecat", c("na.omit"))
#' })
#'
#' @export
ta_strat <- function(dsgroup, stratStrings, outputStrings, .fun = NULL, ...){

if (is.null(stratStrings)){
    output <- .fun(dsgroup, ...)
} else {
    stratV <- dstring(stratStrings)
    stratVars <- stratV(dsgroup)
    ingroup <- dsgroup[,stratStrings, drop = FALSE]

    ap0 <- split(dsgroup, stratVars, drop = TRUE, sep = ";,;", lex.order = TRUE)
    bp0 <- attributes(ap0)
    
    output_length <- length(outputStrings) # or whatever length you want
    output <- vector(mode = "list", length = output_length)
    for(g0 in 1:output_length) {
        names(output)[g0] <- outputStrings[g0]
    }
    for(gi in 1:length(ap0)) {
        outmat <- matrix(0, nrow=1, ncol=length(stratStrings))  
        if (length(stratStrings) == 1) {
            outmat[1,] <- bp0$names[gi]
        } else {
            outmat[1,] <- mystrsplit(bp0$names[gi], ";,;")
        }
        ap2 <- ap0[[gi]]
        res <- .fun(ap2, ...)    
        if (length(res) > 0){
            for(gk in 1:length(res)) { 
                # add the realization of the layered variables to each dataset in res 
                if (!is.null(res[[gk]])){
                    outmat1 <- simple_mat(ingroup, outmat)
                    outmat2 <- NULL
                    for (j in 1:nrow(res[[gk]])){
                        outmat2 <- rbind(outmat2,outmat1)
                    }
                    ap3 <- cbind(outmat2,res[[gk]])
                    output[[gk]] <- rbind(output[[gk]], ap3)    
                }        
            }
        }
    }    
}
return(output)
}

#' Conduct t test  
#'
#' This function immplements t.test after checking that each group in the input dataset has at least 2 non-null records.
#'
#' @param x0 dataframe
#' @param lhs A character string which is name of the main variable of numeric type
#' @param rhs A character string which is name of the group variable of factor type with two levels
#' @param paracode A character vector which is used for the specification of the 4 parameters (alternative, var.equal, conf.level, na.action).
#'    
#' @return an R List which contains two dataframes;  
#'    ttest_result and ttest_equalVariance_check.
#'
#' @examples
#' ttest_wrap(x, "BMI", "Sex", c("two.sided", "TRUE", "0.95", "na.omit"))
#'
#' @export
ttest_wrap <- function(x0, lhs, rhs, paracode) {
    output <- NULL
    spl <- split(x0[lhs], x0[rhs], lex.order = TRUE)
    if (length(spl) > 1) {
        spl1 <- spl[[1]][,lhs]
        spl2 <- spl[[2]][,lhs]
        if (length(na.omit(spl1)) > 1 & length(na.omit(spl2)) > 1){
            outtest <- t.test(x = spl1, y = spl2, alternative = paracode[1], var.equal = as.logical(paracode[2]), conf.level = as.double(paracode[3]), na.action = paracode[4])
            ot1 <- data.frame(statistic=outtest$statistic, parameter=outtest$parameter, p.value=outtest$p.value, conf.int.min=outtest$conf.int[1], conf.int.max=outtest$conf.int[2], mean.x=outtest$estimate[1], mean.y=outtest$estimate[2], null.value=outtest$null.value, stderr=outtest$stderr, alternative=outtest$alternative, row.names = NULL)
labels(ot1) <- c("the value of the t-statistic", "the degrees of freedom for the t-statistic", "the p-value for the test", "the lower limit of the confidence interval", "the upper limit of the confidence interva", "mean of the group 1", "mean of the group 2", "mean difference", "the standard error of the mean difference", "the alternative hypothesis")
      
    sd.1 <- sd(na.omit(spl1))
    sd.2 <- sd(na.omit(spl2))
    lv1 <- data.frame(sd.1, sd.2)
    
    if (sd.1/sd.2 < 2 & sd.2/sd.1 < 2){
        lv1$check <- TRUE
    } else {
        lv1$check <- FALSE
    }
     
    labels(lv1) <- c("standard deviation of group 1", "standard deviation of group 2", "equalVariance_check")

            output <- list(ttest_result = ot1, ttest_equalVariance_check = lv1)
            
        } 
    }
    return(output)
}

#' Conduct paired t test  
#'
#' This function immplements t.test with the parameter paired = TRUE after checking that each group in the input dataset has at least 2 non-null records.
#'
#' @param x0 dataframe
#' @param lhs A character string which is name of the main variable of numeric type
#' @param rhs A character string which is name of the group variable of factor type with two levels
#' @param paracode A character vector which is used for the specification of the 4 parameters (alternative, var.equal, conf.level, na.action).
#'    
#' @return an R List which contains one dataframe;  
#'    pttest_result.
#'
#' @examples
#' pttest_wrap(x, "BMI", "Sex", c("two.sided", "TRUE", "0.95", "na.omit"))
#'
#' @export
pttest_wrap <- function(x0, lhs, rhs, paracode) {
        output <- NULL
        spl1 <- x0[,lhs]
        spl2 <- x0[,rhs]
        if (length(na.omit(spl1)) > 1 & length(na.omit(spl2)) > 1){
            outtest <- t.test(x = spl1, y = spl2, paired = TRUE, alternative = paracode[1], var.equal = as.logical(paracode[2]), conf.level = as.double(paracode[3]), na.action = paracode[4])
            ot1 <- data.frame(statistic=outtest$statistic, parameter=outtest$parameter, p.value=outtest$p.value, conf.int.min=outtest$conf.int[1], conf.int.max=outtest$conf.int[2], mean=outtest$estimate, null.value=outtest$null.value, stderr=outtest$stderr, alternative=outtest$alternative, row.names = NULL)
labels(ot1) <- c("the value of the t-statistic", "the degrees of freedom for the t-statistic", "the p-value for the test", "the lower limit of the confidence interval", "the upper limit of the confidence interva", "mean of the differences", "mean difference", "the standard error of the mean difference", "the alternative hypothesis")

            output <- list(pttest_result = ot1)
            return(output)
        }
}


#' Count N  
#'
#' New version of the length function which can handle NA's: if na.rm==T, it doesn't count them
#'
#' @param x dataframe
#' @param na.rm a character string (TRUE or FALSE) for the parameter na.rm
#'    
#' @return N
#'
#' @examples
#' length2(x, na.rm=TRUE)
# 
length2 <- function (x, na.rm=TRUE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
}

#' Comput mode
#'
#' Comput mode which is the most frequent of all values of a input vector
#'
#' @param x vector
#' @param na.rm a character string (TRUE or FALSE) for the parameter na.rm
#'    
#' @return mode
#'
#' @examples
#' mymode(x, na.rm=TRUE)
#
mymode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

#' Comput summary statistics
#'
#' Create a dataframe whose variables are mean, sd, median, min, max, q1 and q3.
#'
#' @param x dataframe
#' @param var a character string which is the name of a variable
#' @param na.rm a character string (TRUE or FALSE) for the parameter na.rm
#'    
#' @return the dataframeof summary statistics.
#'
#' @examples
#' summary0(x, "Age", na.rm=TRUE)
#
summary0 <- function(x, var, na.rm0) {

    ot1 <- data.frame(N = length2(x[[var]], na.rm=na.rm0),
          mean = mean(x[[var]], na.rm=na.rm0),
          sd = sd(x[[var]], na.rm=na.rm0),
          median = median(x[[var]], na.rm=na.rm0),
          min = min(x[[var]], na.rm=na.rm0),
          max = max(x[[var]], na.rm=na.rm0),
          q1 = quantile(x[[var]], probs = 0.25, na.rm=na.rm0),
          q3 = quantile(x[[var]], probs = 0.75, na.rm=na.rm0),
        row.names = NULL)
    ot1$se <- ot1$sd/sqrt(ot1$N)

    return(ot1)

}

#' Compute summary statistics of a continuous variable 
#'
#' @param x dataframe
#' @param var A character string which is name of the main variable of numeric type
#' @param na.rm0 A character string which is used for the specification of the parameter na.rm.
#'    
#' @return an R List which contains 1 dataframe;  
#'    summary_result.
#'
#' @examples
#' summary_wrap(x, "BMI", "TRUE")
#'
#' @export
summary_wrap <- function(x, var, na.rm0) {

    ot1 <- summary0(x, var, na.rm0)

    labels(ot1) <- c("N", "Mean", "Standard Deviation", "Median", "Min", "Max", "Q1", "Q3", "Standard Error")

     output <- list(summary_result = ot1)
     return(output)

}

#' Compute summary statistics of a categorical variable 
#'
#' @param x dataframe
#' @param var A character string which is name of the main variable of numeric type
#' @param exclude0 A character string which is used for the specification of the parameter exclude of the table function.
#'    
#' @return an R List which contains 1 dataframe;  
#'    summary_result.
#'
#' @examples
#' summaryN_wrap(x, "AgeCat", "c(NA, NaN)")
#'
#' @export
summaryN_wrap <- function(x, var, exclude0) {

    t1 <- table(x[,c(var)], exclude=exclude0)
    ot1 <- as.data.frame(t1)
    colnames(ot1) <- c(var,"N")

    output <- list(summary_result = ot1)
    return(output)

}

#' Conduct chi-square test 
#'
#' This function immplements chisq.test after checking that the number of the row and the number of the column of the contingency table 
#' of the input dataset are both at least 2.
#'
#' @param x0 dataframe
#' @param lhs A character string which is name of the main variable of factor type
#' @param rhs A character string which is name of the group variable of factor type
#' @param correct0 A character vector which is used for the specification of the parameter correct.
#'    
#' @return an R List which contains 3 dataframes;  
#'    chitest_result, chitest_count and chitest_check.
#'
#' @examples
#' chisq_wrap(x, "Sex", "AgeCat", "TRUE")
#'
#' @export
chisq_wrap <- function(x0, lhs, rhs, correct0) {

    table00 <- table(as.integer(x0[,lhs]), as.integer(x0[,rhs]), exclude = c(NA, NaN))
    kr0 <- nrow(table00)
    kc0 <- ncol(table00)
    table01 <- table(x0[,lhs], x0[,rhs], exclude = c(NA, NaN))
    kr <- nrow(table01)
    kc <- ncol(table01)
    output <- NULL
    if (kr0 > 1 & kc0 > 1) {
    chi01 <- chisq.test(x=table01, correct = correct0)
 
    ot1 <- data.frame(statistic=chi01$statistic, parameter=chi01$parameter, p.value=chi01$p.value, row.names = NULL)
    labels(ot1) <- c("the value of the chi-squared test", "the degrees of freedom of the approximate chi-squared distribution", "the p-value for the test")

    ot10 <- chi01$observed
    ot10B <- as.table(ot10,nrow=kr,ncol=kc,dimnames=dimnames(table01))
    ot11 <- as.data.frame(ot10)
    colnames(ot11) <- c(lhs, rhs, "observed")

    ot20 <- chi01$expected
    ot20B <- as.table(ot20,nrow=kr,ncol=kc,dimnames=dimnames(table01))
    ot21 <- as.data.frame(ot20B)
    colnames(ot21) <- c(lhs, rhs, "expected")

    ot30 <- chi01$residuals
    ot30B <- as.table(ot30,nrow=kr,ncol=kc,dimnames=dimnames(table01))
    ot31 <- as.data.frame(ot30B)
    colnames(ot31) <- c(lhs, rhs, "residuals")

    ot40 <- chi01$stdres
    ot40B <- as.table(ot40,nrow=kr,ncol=kc,dimnames=dimnames(table01))
    ot41 <- as.data.frame(ot40B)
    colnames(ot41) <- c(lhs, rhs, "std.residuals")

    ot2 <- data.frame(ot11, expected=ot21[,"expected"], residuals=ot31[,"residuals"], std.residuals=ot41[,"std.residuals"])
    labels(ot2) <- c(lhs, rhs, "the observed counts", "the expected counts under the null hypothesis", "the Pearson residuals: (observed-expected)/sqrt(expected)", "the standardized residuals")

    ck01 <- as.vector(ot20)
    ck02 <- which(ck01>=5)
    ratio <- length(ck02)/length(ck01)
    ot3 <- data.frame(ratio)
    if (ratio >= 0.8){
                ot3$check <- TRUE
            } else {
                ot3$check <- FALSE
            }
    labels(ot3) <- c("the ratio of the expected values which are greater than or equal to 5", "TRUE if at least 80 percent of the expected values are at least 5.")

    output <- list(chitest_result = ot1, chitest_count = ot2, chitest_check = ot3)
    }
    return(output)

}

#' Conduct anova test  
#'
#' This function immplements aov and Anova in car package after checking that each group in the input dataset has at least 2 non-null records 
#' and at least 1 record for each level of the factor of the group variable.
#'
#' @param x0 dataframe
#' @param lhs A character string which is name of the main variable of numeric type
#' @param rhs A character string which is name of the group variable of factor type with multiple levels
#' @param paracode A character vector which is used for the specification of the parameter na.action.
#'    
#' @return an R List which contains 4 dataframes;  
#'    anova1_type1, anova1_type2, anova1_type3 and anova1_equalVariance_check.
#'
#' @examples
#' anova1_wrap(x, "BMI", "AgeCat", c("na.omit"))
#'
#' @export
anova1_wrap <- function(x, lhs, rhs, paracode) {

    l0 <- levels(x[,rhs])
    l1 <- length(l0)
    spl <- split(x[lhs], x[rhs], drop = TRUE, lex.order = TRUE)
    mcon1 <- TRUE
    for (i in 1:length(spl)){
        spli <- spl[[i]][,lhs]
        if (!(length(na.omit(spli)) > 1)){
            mcon1 <- FALSE
        } 
    }
    output <- NULL
    if (length(spl) == l1 & mcon1 == TRUE) {
        result <- aov(x[,lhs]~x[,rhs], data=x, na.action = paracode[1])

        sum <- summary(result)
        type10 <- sum[[1]]
        ot1 <- data.frame(statistic=type10[, "F value"][1], p.value=type10[, "Pr(>F)"][1], SS1=type10[, "Sum Sq"][1], parameter1=type10[, "Df"][1], MS1=type10[, "Mean Sq"][1], SS2=type10[, "Sum Sq"][2], parameter2=type10[, "Df"][2], MS2=type10[, "Mean Sq"][2], row.names = NULL)
labels(ot1) <- c("the value of the F-statistic (Type I test)", "the p-value for the test (Type I test)", str_c("sum-of-squares for ",rhs,sep = ""), str_c("the degrees of freedom for ",rhs,sep = ""), str_c("mean square for ",rhs,sep = ""), "sum-of-squares for residuals", "the degrees of freedom for residuals", "mean square for residuals")

        type20 <- Anova(result, type = "II")
        ot2 <- data.frame(statistic=type20[, "F value"][1], p.value=type20[, "Pr(>F)"][1], SS1=type20[, "Sum Sq"][1], parameter1=type20[, "Df"][1], SS2=type20[, "Sum Sq"][2], parameter2=type20[, "Df"][2], row.names = NULL)
labels(ot2) <- c("the value of the F-statistic (Type II test)", "the p-value (Type II test)", str_c("sum-of-squares for ",rhs,sep = ""), str_c("the degrees of freedom for ",rhs,sep = ""), "sum-of-squares for residuals", "the degrees of freedom for residuals")

        type30 <- Anova(result, type = "III")
        ot3 <- data.frame(statistic1=type30[, "F value"][1], p.value1=type30[, "Pr(>F)"][1], statistic2=type30[, "F value"][2], p.value2=type30[, "Pr(>F)"][2], SS1=type30[, "Sum Sq"][1], parameter1=type30[, "Df"][1], SS2=type30[, "Sum Sq"][2], parameter2=type30[, "Df"][2], SS3=type30[, "Sum Sq"][3], parameter3=type30[, "Df"][3], row.names = NULL)
labels(ot3) <- c("the value of the F-statistic for the intercept and the residuals (Type III test)", "the p-value for the intercept", str_c("the value of the F-statistic for ", rhs, " and the residuals (Type III test)", sep = ""), str_c("the p-value for ",rhs,sep = ""), "sum-of-squares for the intercept", "the degrees of freedom for the intercept", str_c("sum-of-squares for ",rhs,sep = ""), str_c("the degrees of freedom for ",rhs,sep = ""), "sum-of-squares for the residuals", "the degrees of freedom for the residuals")

    for (di in 1:length(spl)){
        sd.i <- sd(na.omit(spl[[di]][,lhs]))
        if (di == 1) {
            colname <- str_c("std",di,sep = "")
            lv1 <- data.frame(sd.i) 
            maxi <- sd.i
            mini <- sd.i
            colnames(lv1) <- c(colname)
        } else {
            colname <- str_c("std",di,sep = "")
            colname1 <- colnames(lv1)
            lv1 <- data.frame(lv1, sd.i) 
            if (sd.i > maxi) maxi <- sd.i
            if (sd.i < mini) mini <- sd.i
            colnames(lv1) <- c(colname1, colname)
        } 
    }

    colname0 <- c(colnames(lv1), "equalVarianceCheck")
    if (maxi/mini < 2){
        lv1$check0 <- TRUE
    } else {
        lv1$check0 <- FALSE
    }
    colnames(lv1) <- colname0
        
    output <- list(anova1_type1 = ot1, anova1_type2 = ot2, anova1_type3 = ot3, anova1_equalVariance_check = lv1)
    }
    return(output)
}

#' Conduct Kruskal-Wallis test for the numeric variable 
#'
#' This function immplements kruskal.test after checking that each group in the input dataset has at least 2 non-null records 
#' and at least 1 record for each level of the factor of the group variable.
#'
#' @param x dataframe
#' @param lhs A character string which is name of the main variable of numeric type
#' @param rhs A character string which is name of the group variable of factor type
#' @param naaction A character vector which is used for the specification of the parameter na.action.
#'    
#' @return an R List which contains 1 dataframe;  
#'    kruskal_result.
#'
#' @examples
#' kruskal_wrap(x, "BMI", "AgeCat", "na.omit")
#'
#' @export
kruskal_wrap <- function(x, lhs, rhs, naaction) {

    l0 <- levels(x[,rhs])
    l1 <- length(l0)
    spl <- split(x[lhs], x[rhs], drop = TRUE, lex.order = TRUE)
    mcon1 <- TRUE
    for (i in 1:length(spl)){
        spli <- spl[[i]][,lhs]
        if (!(length(na.omit(spli)) > 1)){
            mcon1 <- FALSE
        } 
    }
    output <- NULL
    if (length(spl) == l1 & mcon1 == TRUE) {
        outtest <- kruskal.test(x[,lhs]~x[,rhs], data=x, na.action = naaction)
        ot1 <- data.frame(statistic=outtest$statistic, parameter=outtest$parameter, p.value=outtest$p.value, row.names = NULL)
        labels(ot1) <- c("the Kruskal-Wallis rank sum statistic", "the degrees of freedom of the approximate chi-squared distribution", "the p-value for the test")

        output <- list(kruskal_result = ot1)
    }
    return(output)
}

#' Conduct Kruskal-Wallis test for the ordered factor variable 
#'
#' This function immplements kruskal.test after checking that each group in the input dataset has at least 2 non-null records 
#' and at least 1 record for each level of the factor of the group variable.
#'
#' @param x dataframe
#' @param lhs A character string which is name of the main variable of ordered factor type
#' @param rhs A character string which is name of the group variable of factor type
#' @param naaction A character vector which is used for the specification of the parameter na.action.
#'    
#' @return an R List which contains 2 dataframes;  
#'    kruskal_result and kruskal_count.
#'
#' @examples
#' kruskal2_wrap(x, "WeightCat", "AgeCat", "na.omit")
#'
#' @export
kruskal2_wrap <- function(x, lhs, rhs, naaction) {

    l0 <- levels(x[,rhs])
    l1 <- length(l0)
    spl <- split(x[lhs], x[rhs], drop = TRUE, lex.order = TRUE)
    mcon1 <- TRUE
    for (i in 1:length(spl)){
        spli <- spl[[i]][,lhs]
        if (!(length(na.omit(spli)) > 1)){
            mcon1 <- FALSE
        } 
    }
    output <- NULL
    if (length(spl) == l1 & mcon1 == TRUE) {
        outtest <- kruskal.test(as.integer(x[,lhs])~x[,rhs], data=x, na.action = naaction)
        ot1 <- data.frame(statistic=outtest$statistic, parameter=outtest$parameter, p.value=outtest$p.value, row.names = NULL)
        labels(ot1) <- c("the Kruskal-Wallis rank sum statistic", "the degrees of freedom of the approximate chi-squared distribution", "the p-value for the test")

        table01 <- table(x[,lhs], x[,rhs], exclude = c(NA, NaN))
        ot2<- as.data.frame(table01)
        colnames(ot2) <- c(lhs, rhs, "Frequency")
        ot2[,lhs] <- factor(ot2[,lhs], ordered = is.ordered(x[,lhs]), levels = levels(x[,lhs]))
        ot2[,rhs] <- factor(ot2[,rhs], ordered = is.ordered(x[,rhs]), levels = levels(x[,rhs]))

        output <- list(kruskal_result = ot1, kruskal_count = ot2)
    }
    return(output)
}

#' Create dataset for survival curve
#'
#' @param x0 dataframe
#' @param lhs A character string which is name of the variable of numeric type for Time
#' @param rhs A character string which is name of the variable of numeric type for Event (1=target event, 0=censor)
#' @param rhs2 A character string which is name of the group variable of factor type
#' @param naaction A character vector which is used for the specification of the parameter na.action.
#'    
#' @return an R List which contains 2 dataframes;  
#'    survdat and survcen.
#'
#' @examples
#' survival_data(x, "Time", "Event", "AgeCat", c("na.omit"))
#'
#' @export
survival_data <- function(x0, lhs, rhs, rhs2, paracode) {

    # check number of complete records
        x01 <- x0[,c(lhs,rhs,rhs2)]

        n1 <- nrow(x01)
        m1 <- ncol(x01)

        naF <- c()
        for (i in 1:n1){
            naF0 <- 0
            for (j in 1:m1){  
                if (is.na(x01[i,j])){
                    naF0 <- 1
                }
            }
            if (naF0==1) naF <- c(naF,-i)
        }
        if (length(naF)>0){
            x02 <- x01[naF,,drop = FALSE]
        } else {
            x02 <- x01
        }

    output <- NULL
    if (nrow(x02)>0){
    surv_object <- Surv(time = x0[,lhs], event = x0[,rhs])
    if (!is.null(rhs2)){
        
        s <- survfit(surv_object ~ x0[rhs2][[1]], data = x0, na.action = paracode[1])
        len <- length(s$n)

            if (!is.null(s$strata)) {
                groups <- str_sub(names(s$strata[1]), start = 15)
                if (length(s$strata)>1){
                for (i in 2:length(s$strata)){
                    groups <- c(groups, str_sub(names(s$strata[i]), start = 15))
                }
                }
            } else {
                groups <- na.omit(x0[,rhs2])[1]
            }  

            gr.df <- vector('list', len)
            ind <- vector('list', len)
            if (!is.null(s$strata)) {
                n.ind <- c(0,s$strata); n.ind <- cumsum(n.ind)
            } else {
                n.ind <- c(0,length(s$time)); n.ind <- cumsum(n.ind)
            }  
            for(i in 1:len) ind[[i]] <- (n.ind[i]+1):n.ind[i+1]

            for(i in 1:len){
                gr.df[[i]] <- data.frame(
                time = c(0, s$time[ ind[[i]] ]),
                surv = c(1, s$surv[ ind[[i]] ]),
                up = c(1, s$upper[ ind[[i]] ]),
                low = c(1, s$lower[ ind[[i]] ]),
                cens = c(0, s$n.censor[ ind[[i]] ]),
                strata = rep(groups[i], length(s$time[ ind[[i]] ]) + 1))
            }
    } else {
            s <- survfit(surv_object ~ 1, data = x0, na.action = paracode[1])
            len <- length(s$n)

            gr.df <- vector('list', len)
            ind <- vector('list', len)
            n.ind <- c(0,length(s$time))
            n.ind <- cumsum(n.ind)  
            for(i in 1:len) ind[[i]] <- (n.ind[i]+1):n.ind[i+1]

            for(i in 1:len){
                gr.df[[i]] <- data.frame(
                time = c(0, s$time[ ind[[i]] ]),
                surv = c(1, s$surv[ ind[[i]] ]),
                up = c(1, s$upper[ ind[[i]] ]),
                low = c(1, s$lower[ ind[[i]] ]),
                cens = c(0, s$n.censor[ ind[[i]] ]))
            }
    }
    dat <- do.call(rbind, gr.df)
    dat.cens <- subset(dat, cens != 0)
    output <- list(survdat = dat, survcen = dat.cens)
    }
    return(output)
}

#' Conduct survival analysis 
#'
#' @param x0 dataframe
#' @param lhs A character string which is name of the variable of numeric type for Time
#' @param rhs A character string which is name of the variable of numeric type for Event (1=target event, 0=censor)
#' @param rhs2 A character string which is name of the group variable of factor type
#' @param naaction A character vector which is used for the specification of the parameter na.action.
#'    
#' @return an R List which contains 2 dataframes;  
#'    survfit_result and survdiff_result.
#'
#' @examples
#' survival_wrap(x, "Time", "Event", "AgeCat", c("na.omit"))
#'
#' @export
survival_wrap <- function(x0, lhs, rhs, rhs2, paracode) {

    if (!is.null(rhs2)){
        checkStrings <- c(lhs,rhs,rhs2)
    } else {
        checkStrings <- c(lhs,rhs)
    }
    check <- TRUE
            limit0 <- 2
            if (nrow(na.omit(x0[, checkStrings])) < limit0) {
                check <- FALSE
            }

    output <- NULL
    if (check){
    surv_object <- Surv(time = x0[,lhs], event = x0[,rhs])
    if (!is.null(rhs2)){

        outtest <- survfit(surv_object ~ x0[rhs2][[1]], data = x0, na.action = paracode[1])

        if (!is.null(outtest$strata)) {
            strata0 <- rep(str_sub(names(outtest$strata[1]), start = 15), outtest$strata[1])
            if (length(outtest$strata)>1){
            for (i in 2:length(outtest$strata)){
                strata0 <- c(strata0, rep(str_sub(names(outtest$strata[i]), start = 15), outtest$strata[i]))
            }
            }
            ot1 <- data.frame(strata=strata0, time=outtest$time, n.risk=outtest$n.risk, n.event=outtest$n.event, n.censor=outtest$n.censor, surv=outtest$surv, std.err=outtest$std.err, lower=outtest$lower, upper=outtest$upper, row.names = NULL)
            labels(ot1) <- c("strata", "time", "number at risk", "number of event", "number of censor", "survival ratio", "standard error", "lower 95% CI", "upper 95% CI")

            surv_diff <- survdiff(surv_object ~ x0[rhs2][[1]], data = x0, na.action = paracode[1])
            ot2 <- data.frame(statistic=surv_diff$chisq, parameter=length(surv_diff$n)-1, p.value=pchisq(surv_diff$chisq, length(surv_diff$n)-1, lower.tail = FALSE), row.names = NULL)
            labels(ot2) <- c("the chisquare statistic for a test of equality", "the degrees of freedom", "the p-value for the test")
        } else {
            ot1 <- data.frame(strata=rep(na.omit(x0[,rhs2])[1],length(outtest$time)), time=outtest$time, n.risk=outtest$n.risk, n.event=outtest$n.event, n.censor=outtest$n.censor, surv=outtest$surv, std.err=outtest$std.err, 
lower=outtest$lower, upper=outtest$upper, row.names = NULL)
        labels(ot1) <- c("strata", "time", "number at risk", "number of event", "number of censor", "survival ratio", "standard error", "lower 95% CI", "upper 95% CI")

            ot2 <- NULL
        }
    } else {
        fit1 <- survfit(surv_object ~ 1, data = x0, na.action = paracode[1])

        outtest <- summary(fit1)

        ot1 <- data.frame(strata=rep("all",length(outtest$time)), time=outtest$time, n.risk=outtest$n.risk, n.event=outtest$n.event, n.censor=outtest$n.censor, surv=outtest$surv, std.err=outtest$std.err, lower=outtest$lower, upper=outtest$upper, row.names = NULL)
        labels(ot1) <- c("strata", "time", "number at risk", "number of event", "number of censor", "survival ratio", "standard error", "lower 95% CI", "upper 95% CI")

        ot2 <- NULL

    }

    output <- list(survfit_result = ot1, survdiff_result = ot2)

    }
    return(output)
}

#' Conduct Wilcoxon Rank Sum test for numeric variable
#'
#' This function immplements wilcox.test after checking that each group in the input dataset has at least 2 non-null records
#'
#' @param x dataframe
#' @param lhs A character string which is name of the main variable of numeric type
#' @param rhs A character string which is name of the group variable of factor type with two levels
#' @param paracode A character vector which is used for the specification of the 4 parameters (alternative, correct, conf.level, na.action).
#'    
#' @return an R List which contains one dataframe;  
#'    wil_ranksum_result.
#'
#' @examples
#' wil_wrap(x, "BMI", "SEX", c("two.sided", "TRUE", "0.95", "na.omit"))
#'
#' @export
wil_wrap <- function(x0, lhs, rhs, paracode) {
    spl <- split(x0[lhs], x0[rhs], drop = TRUE, lex.order = TRUE)
    output <- NULL
    if (length(spl) > 1) {
        spl1 <- spl[[1]][,lhs]
        spl2 <- spl[[2]][,lhs]
        if (length(na.omit(spl1)) > 1 & length(na.omit(spl2)) > 1){
            outtest <- wilcox.test(x = spl1, y = spl2, alternative = paracode[1], correct = as.logical(paracode[2]), conf.level = as.double(paracode[3]), na.action = paracode[4])
            ot1 <- data.frame(statistic=outtest$statistic, p.value=outtest$p.value, alternative=outtest$alternative, row.names = NULL)
            labels(ot1) <- c("the value of the test statistic", "the p-value for the test", "the alternative hypothesis")

            output <- list(wil_ranksum_result = ot1)
        } 
    }
    return(output)
}

#' Conduct Wilcoxon Rank Sum test for ordered factor variable
#'
#' This function immplements wilcox.test after checking that each group in the input dataset has at least 2 non-null records
#'
#' @param x dataframe
#' @param lhs A character string which is name of the main variable of ordered factor type
#' @param rhs A character string which is name of the group variable of factor type with two levels
#' @param paracode A character vector which is used for the specification of the 4 parameters (alternative, correct, conf.level, na.action).
#'    
#' @return an R List which contains two dataframes;  
#'    wil_ranksum_result and wil_ranksum_count.
#'
#' @examples
#' wil2_wrap(x, "BMI", "SEX", c("two.sided", "TRUE", "0.95", "na.omit"))
#'
#' @export
wil2_wrap <- function(x0, lhs, rhs, paracode) {
    spl <- split(x0[lhs], x0[rhs], drop = TRUE, lex.order = TRUE)
    output <- NULL
    if (length(spl) > 1) {
        spl1 <- as.integer(spl[[1]][,lhs])
        spl2 <- as.integer(spl[[2]][,lhs])
        if (length(na.omit(spl1)) > 1 & length(na.omit(spl2)) > 1){
            outtest <- wilcox.test(x = spl1, y = spl2, alternative = paracode[1], correct = as.logical(paracode[2]), conf.level = as.double(paracode[3]), na.action = paracode[4])
            ot1 <- data.frame(statistic=outtest$statistic, p.value=outtest$p.value, alternative=outtest$alternative, row.names = NULL)
            labels(ot1) <- c("the value of the test statistic", "the p-value for the test", "the alternative hypothesis")

            table01 <- table(x0[,lhs], x0[,rhs], exclude = c(NA, NaN))
            ot2<- as.data.frame(table01)
            colnames(ot2) <- c(lhs, rhs, "Frequency")
            ot2[,lhs] <- factor(ot2[,lhs], ordered = is.ordered(x0[,lhs]), levels = levels(x0[,lhs]))
            ot2[,rhs] <- factor(ot2[,rhs], ordered = is.ordered(x0[,rhs]), levels = levels(x0[,rhs]))

            output <- list(wil_ranksum_result = ot1, wil_ranksum_count = ot2)
        } 
    }
    return(output)
}

#' Conduct Wilcoxon Signed Rank test for numeric variable
#'
#' This function immplements wilcox.test with paired = TRUE after checking that each group in the input dataset has at least 2 non-null records
#'
#' @param x dataframe
#' @param lhs A character string which is name of the main variable of numeric type
#' @param rhs A character string which is name of the group variable of factor type with two levels
#' @param paracode A character vector which is used for the specification of the 4 parameters (alternative, correct, conf.level, na.action).
#'    
#' @return an R List which contains one dataframe;  
#'    wil_signedrank_result.
#'
#' @examples
#' wilp_wrap(x, "BMI", "SEX", c("two.sided", "TRUE", "0.95", "na.omit"))
#'
#' @export
wilp_wrap <- function(x0, lhs, rhs, paracode) {
        output <- NULL
        spl1 <- x0[,lhs]
        spl2 <- x0[,rhs]
        if (length(na.omit(spl1)) > 1 & length(na.omit(spl2)) > 1){
            outtest <- wilcox.test(x = spl1, y = spl2, paired = TRUE, alternative = paracode[1], correct = as.logical(paracode[2]), conf.level = as.double(paracode[3]), na.action = paracode[4])
            ot1 <- data.frame(statistic=outtest$statistic, p.value=outtest$p.value, alternative=outtest$alternative, row.names = NULL)
            labels(ot1) <- c("the value of the test statistic", "the p-value for the test", "the alternative hypothesis")

            output <- list(wil_signedrank_result = ot1)
        }
        return(output)
}

#' Conduct Wilcoxon Signed Rank test for ordered factor variable
#'
#' This function immplements wilcox.test with paired = TRUE after checking that each group in the input dataset has at least 2 non-null records
#'
#' @param x dataframe
#' @param lhs A character string which is name of the main variable of ordered factor type
#' @param rhs A character string which is name of the group variable of factor type with two levels
#' @param paracode A character vector which is used for the specification of the 4 parameters (alternative, correct, conf.level, na.action).
#'    
#' @return an R List which contains one dataframe;  
#'    wil_signedrank_result.
#'
#' @examples
#' wilp2_wrap(x, "BMI", "SEX", c("two.sided", "TRUE", "0.95", "na.omit"))
#'
#' @export
wilp2_wrap <- function(x0, lhs, rhs, paracode) {
        output <- NULL
        spl1 <- as.integer(x0[,lhs])
        spl2 <- as.integer(x0[,rhs])
        if (length(na.omit(spl1)) > 1 & length(na.omit(spl2)) > 1){
            outtest <- wilcox.test(x = spl1, y = spl2, paired = TRUE, alternative = paracode[1], correct = as.logical(paracode[2]), conf.level = as.double(paracode[3]), na.action = paracode[4])
            ot1 <- data.frame(statistic=outtest$statistic, p.value=outtest$p.value, alternative=outtest$alternative, row.names = NULL)
            labels(ot1) <- c("the value of the test statistic", "the p-value for the test", "the alternative hypothesis")

            output <- list(wil_signedrank_result = ot1)
        }
        return(output)
}



