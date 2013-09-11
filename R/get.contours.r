
#' Calculate contours of pair (n1, n2) with a given minimal power 
#' 
#' @aliases get.contours
#' 
#' @param z A data frame produced by \code{power.grid()}
#' @param power Power of the test
#' @return A data frame combinations of (n1, n2)
#' @export
get.contours <- function(z, power = 0.8){
    zs <- split(z, as.factor(z$percent))
    dn <- dimnames(zs[[1]])
    contours <- matrix(NA, ncol = length(zs), nrow = length(unique(z$n2)))
    powers <- matrix(NA, ncol = length(zs), nrow = length(unique(z$n2)))
    dimnames(powers) <- dimnames(contours) <- list(unique(z$n2), names(zs))
    ran <- range(zs[[1]]$n1)
    for (i in 1:length(zs)) {
        v <- zs[[i]]
        pct <- v[1,"percent"]
        comparison <- table(v$n1, v$n2, v$power > power)
        
        ## a hack to handle dropped third dimension when
        ## all are TRUE or FALSE
        # if (dim(comparison)[3] == 2) 
        xx <- comparison[,,1] # select FALSE
        if (all(xx == 1) & v$power[1] > power) xx <- 1 - xx
        
        idx <- rowSums(xx)
        contours[, i] <- idx + ran[1]
        for (s in 1:nrow(contours)) {
            flags <- v$n1 == (s + ran[1] - 1) & v$n2 == contours[s, i]
            powers[s, i] <- ifelse(sum(flags) == 1, v[flags, "power"], NA)
        }
    }
    # browser()
    contours[contours > ran[2]] <- NA
    return(list(contours = contours,
                powers = powers))
}
