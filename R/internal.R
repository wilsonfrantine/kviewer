# Internal functions and imports
#- Import Blocok --------------
#' @import stats
#' @import ggplot2
#' @import cowplot
#' @importFrom fpc pamk

#- Color Pallets
#----Seting Collor Pallets
#Okabe-Ito Collors with no Black
okabeito_colors <- c('orange' = "#E69F00", 'sky blue' = "#56B4E9", 'bluish green' = "#009E73", 'yellow' = "#F0E442", 'blue' = "#0072B2",'vermilion' = "#D55E00", 'reddish purple' = "#CC79A7")

#Based in Structure  colors
structure_colors <- c(`slightblue` = "#B0AEFF", `sblue` = "#2020FF", `sred` = "#FF3030", `syellow` = "#FCFC75", `sgreen` = "#00b159", `svermillion` = "#d11141", `spurple` = "#CC79A7", `sblack` = "#000000", `swhite` = "#FFFFFF", `sgrey50` = "#CCCCCC")

#Based in Structure  colors
maverick_colors <- c( `mblue` = "#4573b3", `mred` = "#d72e26", `mwhite`         = "#eee8c2", `myellow` = "#faa166", `mgreen` = "#00b159", `mvermillion` = "#d11141", `mpurple` = "#CC79A7", `mblack` = "#000000", `mgrey50` = "#CCCCCC")

#- Functions Block -----------
## Vary basics #####
#' @name is.zip
#' @keywords internal
#' @noRd
is.zip <- function(filepath){
  result <- tryCatch({
    unzip(filepath, list = TRUE)
    return(TRUE)},
    error = function(e){
    return(FALSE)
  })
  return(result)
}

#' @name grab.integers
#' @description function to return numbers from strings
#' @noRd
#' @keywords internal

grab.integers <- function(x){
  return(as.numeric(gsub(".+ +(-?\\d+) *.*","\\1", x)))
}

#' @name grab.decimals
#' @description function to return numbers from strings
#' @noRd
#' @keywords internal
grab.decimals <- function(x){
  var <- as.numeric(gsub(".+ +(-?\\d+\\.\\d+) *.*","\\1",x))
  return(var)
}
## Execution functions ####
#' @name checkpath
#' @description function exist to check and unzip files called from ext.str.par
#' @param x a file path to a zip file or folder containing structure result log files
#' @noRd
#' @keywords internal
checkpath <- function(x){
  if(dir.exists(x)){
    return(unlist(kviewer:::find.files(x)))
  }else if(is.zip(x)){
    unzip(x, exdir = "results", overwrite = T)
    return(unlist(find.files("results")))
  }else{
    return(dirname(x))
  }
}
find.files <- function(x){
  regex <- "_\\d+_f"

  l.files <- list.files(x, full.names = T)

  if(TRUE %in% grepl(regex, l.files)){
    return(normalizePath( dirname(list.files(x, full.names = T))[1] ))
  } else {
    lapply(l.files[!grepl(regex, l.files)], find.files)
  }
}

#' @name str.ext.par
#' @description a function to read and extract param runs from structure file
#' @param x a file name
#' @noRd
#' @keywords internal
str.ext.par <- function(file){
  filename <- strsplit(file,"/")
  filename <- unlist(filename)[length( unlist(filename) )]
  file <- file.path( file )
  if( file.exists(file) ){
    connection <- readLines(file, warn = F)
    par.row <- grep("^Run parameters:", connection)

    nsamples <- grab.integers(connection[par.row+1])
    nloci    <- grab.integers(connection[par.row+2])

    nRun        <- as.numeric(gsub(".*?(\\d+).*","\\1",filename))
    K           <- grab.integers(connection[par.row+3])
    LnP.of.data <- grab.decimals(connection[startsWith(connection, "Estimated Ln Prob of Data")])
    meanlnLK    <- grab.decimals(connection[startsWith(connection, "Mean value of ln likelihood")])
    sdLnLK      <- grab.decimals(connection[startsWith(connection, "Variance of ln likelihood")])

    new.line <- data.frame( "file"        = filename,
                            "Run"         = nRun,
                            "K"           = K,
                            "LnP.of.data" = LnP.of.data,
                            "meanlnK"     = meanlnLK,
                            "sdlnLK"      = sdLnLK)

    return(new.line)
  }else{
    stop(paste( file.path(file), "doesn't exist") )
  }
}

#' @name qmatrix.reshape
#' @keywords internal
#' @noRd
#' @param m a dataframe from qmatrix function
#' @order an specific order to organize the plot functions
qmatrix.reshape <- function(m=NULL){
  qm <- reshape(data=m, varying = names(m)[-c(1:4)],
          v.names   = "qvalue",
          times     = names(m)[-c(1:4)],
          timevar   = "cluster",
          direction = "long")
  return(qm)
}

#' @name population.names.interpreter
#' @keywords internal
#' @noRd
#' @param input a vector with with designated names. It must be a factor with same number of level as populations
#' @param df a dataframe for correspondence of populations and individuals.
population.names.interpreter <- function (input=NULL, output=NULL, df=NULL){
  if(is.null(input)){
    output<-as.factor(df$pop)
  }else{
    output<-as.factor(input[c(as.factor(df$pop))])
  }
  return(output)
}
