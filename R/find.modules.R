#find.modules
#' @name find.modules
#' @title Find whether and how many modals are in each K-values
#' @description This function still prototype. find different modules for a given K value
#' @param path the path to structure results folder
#'@export
find.modules <- function(path){
  file.list <- list.files(
    path = dirname(file.choose()),
    full.names = T
  )

  m.list <- lapply( file.list, qmatrix)

  import.log <- data.frame(
    file.list,
    "imported.order" = c(1:length(file.list)),
    "n.cluster"      = unlist(lapply(m.list, ncol))-4,
    "module"         = 1
  )

  for(k in 2:max(import.log$n.cluster)){
    l.k.files <- import.log[import.log$n.cluster==k, "imported.order" ]

    temp.list <- m.list[l.k.files]
    for(i in 1:length(temp.list)){
      temp.list[[i]] <- temp.list[[i]][,-c(1:4)]
    }
    cor.list <- matrix(nrow = length(temp.list), ncol = length(temp.list))
    for(i in 1:I(length(temp.list)-1)){
      for(j in I(i+1):length(temp.list)){
        cor.list[[i,j]] <- mean(cancor(temp.list[[i]], temp.list[[j]])[[1]])

      }
    }

    cor.list <- cor.list[-nrow(cor.list),-1]

    bestk<-fpc::pamk(cor.list)
    cluster.list <- bestk$pamobject$clustering
    suppressWarnings(
      import.log$module[l.k.files] <- cluster.list
    )
  }
 list("qmatrix"=m.list, "modules"=import.log)
  import.log <- import.log[
   order(import.log[,"n.cluster"], import.log[,"module"]),
         ]
  return(list("qmatrix"=m.list, "modules"=import.log))
}
