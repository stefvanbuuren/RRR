
#' Run the Shiny app from the console
#' 
#' @export
go <- function(){
    runApp(appDir = file.path(path.package(package = "RRR"), "3R"))
}
