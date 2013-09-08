## initialize.r
## Sets all paths, global variables and libraries

#' Resets the global variables
#' 
#' This function is called upon initialization to set the proper
#' global variables.
#' 
#' @aliases reset.globals
#' @return Nothing. This function is called for its side effects in the \code{.GlobalEnv}.
#' @export
reset.globals <- function () {
    ## initializes global variables
    pkg <- path.package("RRR")
    setwd(file.path(pkg, "3R"))    
}



