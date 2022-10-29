#' Create dataset for survival curve
#'
#' @param x0 dataframe
#' @param lhs A character string which is name of the variable of numeric type for Time
#' @param rhs A character string which is name of the variable of numeric type for Event (1=target event, 0=censor)
#' @param rhs2 A character string which is name of the group variable of factor type
#' @param paracode A character vector which is used for the specification of the parameter na.action.
#'    
#' @return an R List which contains 2 dataframes;  
#'    survdat and survcen.
#'
#' @examples
#' \dontrun{
#' survival_data(demo, "Time", "Event", "AgeCat", c("na.omit"))
#' }
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

