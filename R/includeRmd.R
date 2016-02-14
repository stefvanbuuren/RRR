
#' Compiles Rmd and output html
#'
#' @aliases includeRmd
#' @param file file name of the .Rmd file
#' @param fragment.only logical passed to the knitr::knit2html() function
#' @param quiet Default \code{quiet = TRUE} passed down to \code{knit2html()}
#' @param \dots Additonal arguments passed down to \code{knit2html()}
#' @return html vector
#' @export
includeRmd <- function(file, fragment.only = TRUE, quiet = TRUE, ...) {
    contents <- paste(readLines(file, warn = FALSE), collapse = '\n')
    html <- knitr::knit2html(text = contents,
                             fragment.only = fragment.only,
                             quiet = quiet,
                             ...)
    Encoding(html) <- 'UTF-8'
    return(HTML(html))
}

