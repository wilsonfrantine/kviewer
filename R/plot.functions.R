#plot functions
#'@name plotq
#'@title Ploting the barplot coancestry cofficient
#'@description creates a plot of coancestry coficient with ggplot2
#'@param m a q matrix extracted with qmatrix function
#'@param plot_colors a set of colors iqual to K to color each plot
#'@param pop_names a vector of population names to be ploted
#'@export
plotq <- function(m=NULL, plot_colors=NULL, pop_names=NULL) {
  if(is.null(m)){
    m <- qmatrix.reshape(qmatrix())
  }
  if(is.null(plot_colors)){
    plot_colors <- c( `mblue` = "#4573b3", `mred` = "#d72e26", `mwhite`         = "#eee8c2", `myellow` = "#faa166", `mgreen` = "#00b159", `mvermillion` = "#d11141", `mpurple` = "#CC79A7", `mblack` = "#000000", `mgrey50` = "#CCCCCC", `extra` = "#CCBB22")
  }
  if(!is.null(pop_names)){
    m$pop <- as.factor(m$pop)
    levels(m$pop) <- pop_names
  }
  plotq <-
    ggplot(data=m, aes(x=read, y=qvalue, fill=cluster))+
    geom_bar(position = "stack", stat = "identity", width = 1)+
    facet_grid(.~ pop, scale="free_x", space="free_x", switch = "x")+
    scale_fill_manual(values= as.vector(plot_colors))+
    scale_x_discrete(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    labs(y=paste0("K", nlevels(as.factor(m$cluster))))+
    theme(
      strip.background = element_blank(),
      panel.background = element_rect(colour = "black"),
      panel.grid       = element_blank(),
      panel.border = element_rect(fill = NA, colour = "black"),
      axis.title.y = element_text(face="bold.italic"),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size=10),
      panel.spacing.x = unit(0, "cm"),
      legend.position = "none"
    )

  return(plotq)
}

#'@name plot_estimators
#'@title Ploting Best-K Estimators
#'@description return a list with ggplot2 plots for all estimators
#'@param m a table returned from calc.estimators function
#'@export
plot_estimators <- function(m){
  #meanLk+sdLk
  meanLK <-ggplot(m, aes(K, meanLk, group=1))+
    geom_point(color=alpha("blue",0.5))+
    geom_line(color=alpha("blue",0.5))+
    geom_errorbar(aes(x=K, ymin= meanLk-sdLk,
                      ymax= meanLk+sdLk), width=0, color=alpha("blue",0.5))+
    theme_classic()
  #pK
  pK <- ggplot(m, aes(K, pK, group=1))+
    geom_point(color=alpha("blue",0.5))+
    geom_line(color=alpha("blue",0.5))+
    theme_classic()

  #L1K
  L1K <- ggplot(m, aes(K, L1K, group=1))+
    geom_point(color=alpha("blue",0.5))+
    geom_line(color=alpha("blue",0.5))+
    theme_classic()

  #L2K
  L2K <- ggplot(m, aes(K, L2K, group=1))+
    geom_point(color=alpha("blue",0.5))+
    geom_line(color=alpha("blue",0.5))+
    theme_classic()

  #DeltaK plot
  DeltaK <-ggplot(m, aes(K, DeltaK, group=1))+
    geom_point(color=alpha("blue",0.5))+
    geom_line(color=alpha("blue",0.5))+
    theme_classic()
  return(list(meanLK, pK, DeltaK, L1K, L2K))
}
