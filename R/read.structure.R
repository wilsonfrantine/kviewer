#' @name read.structure
#' @title Reading structure output
#' @description reads files from a structure run and returns a summary table
#' @details this is the most primary basic function.
#' @param x a directory path or a zip file containing the result log
#'  files from structure runs
#' @export
read.structure <- function(x){
  files <- list.files(checkpath(x), pattern = "_f$", full.names = T)

  pars <- data.frame()
  for(i in 1:length(files)){
    pars <- rbind(pars, str.ext.par(files[[i]]))
  }
  pars$K <- as.factor(pars$K)
  return(pars)
}
