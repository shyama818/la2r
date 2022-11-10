#' Delete redundant columns
#'
#' Start with i = 1 and an integer vactor Del with size = 0;
#' if all rows of the column i of the training dataset are identical, append i to Del.
#' if (i > 1) and the rank of the training dataset from the column 1 to the column i - 1 and the rank of the training dataset from the column 1 to the column i are same,
#' append i to Del.
#' Delete the columns in Del of training dataset, test dataset and validation dataset.
#'
#' @param train0 dataframe: training dataset
#' @param test0 dataframe: test dataset
#' @param var0 dataframe: validation dataset
#' @param start0 An integer vector: start0 returned by spl_factor
#' @param start1 An string vector: start1 returned by spl_factor
#' @param start2 An integer vector: start2 returned by spl_factor
#'
#' @return R List with the following 6 objects  
#' 1. train1 dataframe: training dataset after deleting the columns
#' 2. test1 dataframe: test dataset after deleting the columns
#' 3. var1 dataframe: validation dataset after deleting the columns
#' 4. olabel a string vactor: names of the original column names with no duplication
#' 5. odimno a integer: the size of olabel
#' 6. start an integer vactor: a vector which shows the starting position of the original columns
#'        It is constructed as follows: starting with the integer vector c(1), and adding the size of each variable expanded by the one-hot Encoding Scheme.   
#'
#' @examples
#' \dontrun{ 
#'modify_spl(traindata0,testdata0,vardata0,c(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0),
#'c("Bssys","Bsdia","Blwt","Blht","AgeCat","Wt2Cls","Sex","treatment","Elig","Elig","Elig","Elig","Crclct","Crclct","Crclct"),
#'c("Bssys","Bsdia","Blwt","Blht","AgeCat","Wt2Cls","Sex","treatment","Elig___1","Elig___2","Elig___3","Elig___4","Crclct___1","Crclct___2","Crclct___3"))
#' }
#' 
#' @export 
modify_spl <- function(train0, test0, var0, start0, start1, start2){

dsgroup0 <- t(train0)
colname0 <- start2
delF <- c()
for (gk in 1:ncol(dsgroup0)){
    if (length(unique(dsgroup0[,gk])) == 1){
        delF <- c(delF,-gk)
        p0 <- paste(colname0[gk], "will be removed from the analysis because values in the column are all identical. ", sep = " ")
            print(p0)
    } else if (gk > 1) {
        gj <- gk-1
        rank0 <- qr(dsgroup0[,1:gj])$rank 
        rank1 <- qr(dsgroup0[,1:gk])$rank
        if (rank1 == rank0){
            delF <- c(delF,-gk)
            p1 <- paste(colname0[gk], "will be removed from the analysis because it gives the redundant information. ", sep = " ")
            print(p1)
        }
    }   
}
if (length(delF)>0){
    train1 <- train0[delF,]
    test1 <- test0[delF,]
    var1 <- var0[delF,]
    start02 <- start0[delF]
    start12 <- start1[delF]
    start22 <- start2[delF]
} else {
    train1 <- train0
    test1 <- test0
    var1 <- var0
    start02 <- start0
    start12 <- start1
    start22 <- start2
}

olabel <- unique(start12)
odimno <- length(olabel)

xcount <- rep(0,odimno)
for (gk in 1:odimno){
    name <- olabel[gk]
    for (gj in 1:length(start12)){
        cname <- start12[gj]
        if (name == cname) {
            xcount[gk] <- xcount[gk]+1
        }
    }
}
start <- c(1)
for (gk in 1:odimno){
    sind <- length(start)
    start <- c(start, start[sind] + xcount[gk])
}

result = list(train = train1, test = test1, var = var1, olabel = olabel, odimno = odimno, start = start)
return(result)

}

