#' Output the levels of the factor variables
#'
#' Output the levels of the factor variables in the directory of levelpath. 
#'
#' @param stratStrings A character vector of the names of variables
#' @param stratVars A list whose objects are the variables
#' @param levelpath A character string which is the path in which the levels of the factor variable is saved
#'
#' @examples
#' \dontrun{
#' levelfile(lstrings, listVars, "/home/shinichi/workspace/Repository/___project/test/Tree/testTree/BMI/TRTGRP/SEX/anova/anova_type3/table11/___info/___levels")
#' }
#'
#' @export
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

