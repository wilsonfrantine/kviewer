#' @name kview
#' @title Vizualize structure results
#' @description return a list with the main results of the pipeline.
#' @param dir a place or a zip file with structure results
#' @param pop_names a vector given the names of populations to be printed. It must be in same order and number as inputed populations in structure files.
#' @usage kview(dir, pop_names)
#' @export
kview <- function(dir=NULL, pop_names = NULL){
  res <- list()
  if(is.null(dir)){
    dir <- file.choose()
  }
  if(dir.exists(dir)){
    dir <- checkpath(dir) 
    setwd(dir)
  }else if(file.exists(dir)){
    if(kviewer:::is.zip(dir)){
      extdir <- normalizePath(dirname(dir))
      unzip(dir, exdir = paste0(extdir,"/results"), overwrite = T)
      setwd(paste0(extdir,"/results"))
    }else{
      dir <- checkpath(dir) 
      setwd(dir)
    }
  }

  
  res["reads"][[1]]           <- read.structure(dir)
  res["estimators"][[1]]      <- calc.estimators(res$reads[])
  res["qplots"][[1]]          <- find.best.to.plot(res$reads[], pop_names=pop_names)
  res["estimators_plot"][[1]] <- plot_estimators(res$estimators[])
  
  p1 <- cowplot::plot_grid(plotlist = res$qplots, align = "h", axis = "x", 
                     ncol=1)
  p2 <- cowplot::plot_grid(plotlist= res$estimators_plot, nrow=2)
  suppressWarnings(print(p1))
  suppressWarnings(print(p2))
  return(res)
}
