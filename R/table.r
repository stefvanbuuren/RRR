# table.r

#' Calculate table of pair (n1, n2) with a given minimal power 
#' 
#' @aliases get.table
#' 
#' @param powertables A list produced by the \code{calculate.powertables()}
#' @param k number of active treatment groups to be tested
#' @return A data frame combinations of (n1, n2)
#' @export
#' 
get.table <- function(powertables, k = 3){
    powertables <<- powertables
    n.induced <- as.numeric(names(powertables$control.ctrs$contours[, "100"]))
    n.treated <- as.vector(powertables$treated.ctrs$contours)
    n.control <- as.vector(powertables$control.ctrs$contours[,"100"])
    n.total <- n.induced + n.control + k * n.treated

    powerIC <- as.vector(powertables$control.ctrs$powers[, "100"])
    powerIT <- as.vector(powertables$treated.ctrs$powers)

    result <- data.frame(Total = n.total,
                         Induced = n.induced,
                         Control = n.control,
                         Treated = n.treated,
                         Power.IC = powerIC,
                         Power.IT = powerIT)
#                         p[, c("alpha", "power", "percent", "d", "sd")]
#                         )
    result <- result[order( result[, "Total"]), ]
    # result <- decimals(result, c(0, 0, 0, 0, 2, 2))
    # result <- as.character(result)
    # result$treais.na(result$treated)
    return(result)
}

## 
## summary table
## top row
## two group t-test with unequal variances
## musd (3 x 2) matrix
## alpha = alpha, beta = 1 - power, power = power
## direction = one-sided
## scale = original / logged
## number of induced groups = 1
## number of control groups = 1
## number of treated groups = ntreated
## percent reduction = 10 to 100 by 10
## outcome = 
# 
# n n(induced) n(control) n(treated)  alpha power
# 10  5          3          3 0.05    0.8
# 11  6
# 12  7
# 13
# 14
# ..

decimals <- function(df, dec){
    format.num <- function(df, j, dec) {
        col <- df[, j]
        if (is.numeric(col)) {
            format <- paste('%.', dec[j], 'f', sep = "")
            sprintf(format, col)
        }
        else
            col
    }
    nc <- ncol(df)
    if (length(dec) != nc) dec <- rep(dec, nc)
    df2 <- df
    for (j in 1:nc) df2[, j] <- format.num(df, j, dec)
    df2
}
