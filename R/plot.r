

#' Calculate the power of t-test with unequal variances
#' 
#' @aliases power.grid
#' 
#' @param n1 Vector of samples sizes for untreated, diseased group
#' @param n2 Vector of samples sizes for comparison group
#' @param mu1 Mean of outcome for untreated group
#' @param sd1 Standard deviation of outcome for untreated group
#' @param mu2 Mean of outcome for comparison group
#' @param sd2 Standard deviation of outcome for comparison group
#' @param alternative String indicating one- or two sided test: "two.sided", 
#'        "less" or "greater". Default "two.sided"
#' @param percent vector indicating for which percentages of effect size the 
#'        power curves should be calculated 
#' @param alpha Significance level of the test
#' @param power Power of the test
#' @param \dots Additional arguments passed to \code{pwr.t2n.test()}
#' @return A data frame with rows corresponding to different designs
#' @export
power.grid <- function(n1 = 2:30, 
                       n2 = 2:30, 
                       mu1 = 0,
                       mu2 = 1,
                       sd1 = 1,
                       sd2 = 1,
                       alternative = c("two.sided", "less", "greater"),
                       percent = 100,
                       alpha = 0.05,
                       power = 0.80, 
                       ...) {
    alternative <- match.arg(alternative)
    
    mean.diff <- mu2 - mu1
    nn <- expand.grid(n1, n2, percent)
    nn1 <- nn[,1]
    nn2 <- nn[,2]
    pct <- nn[,3]
    
    # unequal groups sd and d
    sd <- sqrt((sd1^2 * (nn1 - 1) + sd2^2 * (nn2 - 1)) / (nn1 + nn2 - 2))
    d <- (mu2 - mu1) / sd
    d <- d * pct / 100
    
    pwr <- pwr.t2n.test(n1 = nn1, 
                        n2 = nn2, 
                        d = d, 
                        sig.level = alpha,
                        power = NULL,
                        alternative = alternative, 
                        ...)
    
    output <- data.frame(n1 = nn1, n2 = nn2, 
                         d = d, sd = sd, percent = pct,
                         power = pwr$power, alpha = alpha, 
                         alternative = alternative)
    return(output)
}

#' Create the chart with power lines
#' 
#' @aliases powerplot
#' 
#' @param powertables list of calculated power on a grid on n1, n2 and percent
#' @param power Power of the test
#' @param main Text for title
#' @param isPBS Logical indicating the the x-axis is PBS or compound group
#' @param \dots Additional arguments passed to \code{eqscplot()}
#' @return NULL
#' @export
powerplot <- function(powertables, power = 0.8, main = NULL, isPBS = TRUE, ...) {
    xlab <- ifelse(isPBS, "n (control)", "n (treated)")
    # cnt <- get.contours(z, power = power)
    cnt <- powertables$treated.ctrs$contours
    # browser()
    if (isPBS) cnt <- powertables$control.ctrs$contours
    x <- as.numeric(dimnames(cnt)[[1]])
    y <- powertables$treated.ctrs$contours
    xy <- na.omit(cbind(x = x, y = y))
    x <- xy[,1]
    y <- xy[,2]
    eqscplot(x = c(0, 30), y = c(0, 30), xlim = c(0, 30), ylim = c(0, 30), 
             col = "transparent", xlab = xlab, ylab = "n (induced)",
             main = main, ...)
    xy <- xy.coords(x = c(min(x), x, 35, 35),
                    y = c(35, y, min(y), 35))
    polygon(x = xy, col = rgb(207,232,207,maxColorValue=255), border = NA)
    xy <- xy.coords(x = c(-5, min(x), x, 35, 35, -5),
                    y = c(35, 35,     y, min(y), -5, -5))
    polygon(x = xy, col = "grey99", border = NA)
    abline(h = seq(0,30,5), v = seq(0,30,5), lty = 2, col = "grey") 

    x <- as.numeric(dimnames(cnt)[[1]])
    matlines(y = cnt[], x = x, lwd = 2, lty = 1,
             col = rev(c("black","red","green","brown","blue","violet","pink","lightblue",
                         "grey","darkred")))
    matpoints(y = cnt[], x = as.numeric(rownames(cnt)), lwd = 2, 
              pch = 21, bg = c("darkred", rep("white", 9)), 
              col = rev(c("black","red","green","brown","blue","violet","pink","lightblue",
                          "grey","darkred")))
    return(NULL)
}

