#Qmatrix builder
#' @name qmatrix
#' @title Extract Coancestry cofficient
#' @description extract Q information for each K
#' @param file a file path to one of the structure runs
#' @param ... parameters to be used with other functions
#' @export
qmatrix <- function(file=NULL, ...){
  if(is.null(file)){ file<-file.choose() }
  run_file<-readLines(con = file, n = -1, ok = T,encoding = "UTF-8", warn = F, skipNul = T)

  start.line <- grep("Inferred ancestry of individuals:", run_file)+2
  param.line <- grep("Run parameters", run_file)
  n.samples  <- grab.integers(run_file[param.line+1])
  n.clusters <- grab.integers(run_file[param.line+3])

  qmatrix<-data.frame()

  for(line in start.line:I(start.line+n.samples)){
      this.line<-gsub("\\(|\\)","", run_file[line])
      this.line<-strsplit(gsub("^\\s+|:","", this.line), " +")[[1]]
      qmatrix<-rbind(qmatrix, as.numeric(this.line))
    }
  colnames(qmatrix)<-c("read","id","miss.data","pop",sprintf("cluster%s", seq(1:n.clusters)))
  return(qmatrix)
}
