#' @name find.best.to.plot
#' @title Finding the Best replicates to plot
#' @description screen the output of read.structure function and uses the lnPr of data to get the file name and plot the correspondent qmatrix
#' @param m the output from the read.structure function
#' @param ... parameters to be passed to nested functions
#' @details this function requires that the work directory would be the same of the structure output file.
#' @export
find.best.to.plot <- function(m, ...){
  res <- list()
  temp <- m[order(m$K, m$LnP.of.data),]
  for(i in 1:nlevels(m$K)){
   temp.m <- qmatrix.reshape(
      qmatrix(
        temp[temp$K==i,"file"][1]
      )
    )
   res[[i]] <- plotq(temp.m, pop_names = ...)
  }
  return(res)
}
