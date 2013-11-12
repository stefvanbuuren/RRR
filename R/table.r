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
                         Power_IC = powerIC,
                         Power_IT = powerIT)
    result <- result[order( result[, "Total"]), ]
    result <- decimals(result, c(0, 0, 0, 0, 2, 2))
    result <- edits(result)
    return(result)
}

#' Calculate table of pair (n1, n2) with a given minimal power for symmetric design
#' 
#' @aliases get.table.symmetric
#' 
#' @param powertables A list produced by the \code{calculate.powertables()}
#' @param k number of active treatment groups to be tested
#' @return A data frame combinations of (n1, n2)
#' @export
#' 
get.table.symmetric <- function(powertables, k = 3){
    powertables <<- powertables

    n.induced <- powertables$sym.control$n1
    n.treated <- powertables$sym.treated$n2
    n.control <- powertables$sym.control$n2
    powerIC <- powertables$sym.control$power
    powerIT <- powertables$sym.treated$power
    n.total <- n.induced + n.control + k * n.treated
    
    idx <- powertables$sym.has.enough.power
    if (any(idx))
        result <- data.frame(Total = n.total[idx][1],
                         Induced = n.induced[idx][1],
                         Control = n.control[idx][1],
                         Treated = n.treated[idx][1],
                         Power_IC = powerIC[idx][1],
                         Power_IT = powerIT[idx][1])
    else 
        result <- data.frame(Total = "-",
                             Induced = ">30",
                             Control = ">30",
                             Treated = ">30",
                             power_IC = "-",
                             power_IT = "-")

    result <- decimals(result, c(0, 0, 0, 0, 2, 2))
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

edits <- function(df){
    df[[1]][df[[1]] == "NA"] <- "-"
    df[[2]][df[[2]] == "NA"] <- ">30"
    df[[3]][df[[3]] == "NA"] <- ">30"
    df[[4]][df[[4]] == "NA"] <- ">30"
    df[[5]][df[[5]] == "NA"] <- "-"
    df[[6]][df[[6]] == "NA"] <- "-"
    df
}
