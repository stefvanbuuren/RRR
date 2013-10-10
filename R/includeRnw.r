
#' Compiles Rmd and output html
#' 
#' @aliases includeRmw
#' @param path file name of the .Rnw file
#' @param envir the environment in which chucks are evaluated
#' @param fragment.only logical passed to the knit2html() function
#' @return html vector
#' @export
includeRmd <- function(path, envir = parent.frame(),
                       fragment.only = TRUE){
    if (!require(knitr))
        stop("knitr package is not installed")
    if (!require(markdown))
        stop("Markdown package is not installed")
    shiny:::dependsOnFile(path)
    contents <- paste(readLines(path, warn = FALSE), collapse = '\n')
    html <- knitr::knit2html(text = contents, envir = envir, fragment.only = fragment.only)
    Encoding(html) <- 'UTF-8'
    return(HTML(html))
}

