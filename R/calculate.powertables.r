# calculate.powertables.r
# SvB 8sep2013

#' Calculates the powertables for the given mu and sd matrix musd
#' and other settings 
#' 
#' @aliases calculate.powertables
#' 
#' @param musd A matrix with three rows and 2 columns, with means and sd per scenario
#' @param alternative String to specify one-sided or two-sided testing
#' @param alpha Type-1 error level
#' @param power Power of the test, or 1 - type-2 error
#' @return A list with four components
#' @export
calculate.powertables <- function(
    musd = NULL,
    alternative = "less",
    alpha = 0.05,
    power = 0.8)
{
    treated <- power.grid(mu1 = musd[1,1], sd1 = musd[1,2],
                          mu2 = musd[3,1], sd2 = musd[3,2],
                          percent = 100, 
                          alternative = "less", 
                          alpha = alpha,
                          power = power)
    treated.ctrs <- get.contours(treated, power = power)

    control <- power.grid(mu1 = musd[1,1], sd1 = musd[1,2],
                          mu2 = musd[2,1], sd2 = musd[2,2],
                          percent = seq(100, 10, -10), 
                          alternative = "less", 
                          alpha = alpha,
                          power = power)
    control.ctrs <- get.contours(control, power = power)
    
    return(list (control = control, control.ctrs = control.ctrs, 
                 treated = treated, treated.ctrs = treated.ctrs,
                 alpha = alpha, power = power, 
                 alternative = alternative))
}


#' Returns the musd matrix from the inputs
#' 
#' @aliases get.musd
#' 
#' @param outcome A string, either \code{hist} or \code{cola}
#' @param hist.induced.mu Mean of the induced group, histology
#' @param hist.induced.sd SD of the induced group, histology
#' @param hist.control.mu Mean of the control group, histology
#' @param hist.control.sd SD of the control group, histology
#' @param hist.treated.mu Mean of the treated group, histology
#' @param hist.treated.sd SD of the treated group, histology
#' @param cola.induced.mu Mean of the induced group, collagen
#' @param cola.induced.sd SD of the induced group, collagen
#' @param cola.control.mu Mean of the control group, collagen
#' @param cola.control.sd SD of the control group, collagen
#' @param cola.treated.mu Mean of the treated group, collagen
#' @param cola.treated.sd SD of the treated group, collagen
#' @return A data frame of 3 rows and 2 columns 
#' @export
get.musd <- function(outcome,
                     hist.induced.mu, hist.induced.sd,
                     hist.control.mu, hist.control.sd,
                     hist.treated.mu, hist.treated.sd,
                     cola.induced.mu, cola.induced.sd,
                     cola.control.mu, cola.control.sd,
                     cola.treated.mu, cola.treated.sd) 
{
    musd <- matrix(NA, nrow = 3, ncol = 2, 
                   dimnames = list(c("Induced","Control","Treated"), c("mu","sd")))
    if (outcome == "hist") {
        musd[1,] <- c(hist.induced.mu, hist.induced.sd)
        musd[2,] <- c(hist.control.mu, hist.control.sd)
        musd[3,] <- c(hist.treated.mu, hist.treated.sd)
    }
    if (outcome == "cola") {
        musd[1,] <- c(cola.induced.mu, cola.induced.sd)
        musd[2,] <- c(cola.control.mu, cola.control.sd)
        musd[3,] <- c(cola.treated.mu, cola.treated.sd)
    }
    return(musd)
}

