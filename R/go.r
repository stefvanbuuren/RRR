
#' Run the Shiny app from the console
#' 
#' @aliases go()
#' @param ... options passed down to \code{runApp}
#' @export
go <- function(...){
    runApp(appDir = file.path(path.package(package = "RRR"), "3R"), ...)
}
