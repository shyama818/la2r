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
#' @param ... other arguments passed on to .fun
#'    
#' @return an R List which contains the output datasets.  
#'
#' @examples
#' \dontrun{
#' ta_strat(demo, c("Treatment", "Sex"), c("Ethnic"), .fun = function(xx) {
#' anova1_wrap(xx, "BMI","Agecat", c("na.omit"))
#' })
#' }
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

