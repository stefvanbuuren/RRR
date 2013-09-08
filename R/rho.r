##
## 

library("nCDunnett")

rho <- function(n = c(20, 10, 10, 10, 10, 10)){
    ## Rafter et al 2002, p. 273
    result <- matrix(NA, nrow = length(n), ncol = length(n))
    for (i in 1:length(n)){
        for (j in 1:length(n)){
            result[i,j] <- sqrt(n[i]*n[j]/((n[i]+n[1])*(n[j]+n[1])))
        }
    }
    result[1,]
}
                
dunnett.t <- function(p, nu, size = c(10, 10, 10, 10, 10), 
                     two.sided = FALSE, ...){
    rho <- rho(size)
    delta <- rep(0, length(rho))
    t <- qNCDun(p = p, nu = nu, rho = rho, delta = delta, 
                two.sided = two.sided, ...)
    round(t, 2)
}

# dunnett.t(c(0.95, 0.99), 6, size = c(20, 10, 10, 10, 10))
    

