# table.r
#

get.table <- function(musd, ncompound, alpha = 0.05, power = 0.80){
    z1 <- power.grid(mu1 = musd[1,1], sd1 = musd[1,2],
                    mu2 = musd[2,1], sd2 = musd[2,2],
                    percent = 100, 
                    alternative = "less", 
                    alpha = alpha,
                    power = power)
    z2 <- power.grid(mu1 = musd[1,1], sd1 = musd[1,2],
                     mu2 = musd[3,1], sd2 = musd[3,2],
                     percent = 100, 
                     alternative = "less", 
                     alpha = alpha,
                     power = power)
    c1 <- get.contours(z1, power = power)
    c2 <- get.contours(z2, power = power)
    # ngrid <- expand.grid(c1$n1, c1$n2, c2$n2)
    table <- z2
    print(table)
    return(table)
}

