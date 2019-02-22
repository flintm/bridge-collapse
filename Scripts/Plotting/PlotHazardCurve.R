# Lightweight plotting function for return period hazard curve
# Madeleine Flint, 2019

PlotHazardCurve <- function(T.delta){
  require(ggplot2)
  require(reshape2)
  require(RColorBrewer)
  if(!("getTheme" %in% ls(.GlobalEnv))) source(file.path("Scripts","Plotting","getTheme.R"))
  
  T.delta.melt <- melt(T.delta, id.vars = "T_0")
  colors <- brewer.pal(11, "RdBu")
  names(colors) <- levels(T.delta.melt$variable)
  pT <- ggplot(data=T.delta.melt, aes(x=T_0, y=value, group=variable, color=variable)) + geom_path() + 
    scale_x_log10() + scale_y_log10() + labs(x=expression(T[R]), y = expression(lambda(T[R])),
                                             title = "Scaled Return-Period Hazard Curve") +
    scale_color_manual(values = colors, name = expression(delta)) + 
    getTheme("NOTE",FALSE) + theme(plot.margin = unit(c(1,1,1,1), "cm"))
  return(pT)
}