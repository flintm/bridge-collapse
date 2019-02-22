# Fig. 4a for "Historical Analysis of Hydraulic Bridge Collapses in the Continental United States"
# Plots showing magnitude of failure and max flows
# Copyright Madeleine Flint, 2016
PlotFailMaxFlows <- function(BridgesDataFrame, SAVE = FALSE, plotTypes = "pLogD", Q_units = "m3s",
                             ONLY = NA, LEGEND = TRUE, LABELS = TRUE, outputType = "PRINT",
                             MAIN_FIELDS = c("Build","Fail","State"),
                             SUPER_FIELDS = c("Hurr","Area"), SEGMENTS = TRUE,
                             SIZE = c(7,11), embedFonts = FALSE, gs_path = "/usr/local/bin/gs"){
  require(ggplot2)
  require(grid)
  require(gridExtra)
  require(plyr)
  require(reshape2)
  if(!("MakeBridgeLabel" %in% ls(.GlobalEnv))) source(file.path("Scripts","Plotting","MakeBridgeLabel.R"))
  if(!("MakeGridPlot" %in% ls(.GlobalEnv))) source(file.path("Scripts","Plotting","MakeGridPlot.R"))
  if(!("colorsP" %in% ls(.GlobalEnv))) source(file.path("Scripts","Plotting","SetupEncoding.R"))
  if(!("getTheme" %in% ls(.GlobalEnv))) source(file.path("Scripts","Plotting","getTheme.R"))
  
  # LABELS
  BridgesDataFrame$LABEL <- MakeBridgeLabel(BridgesDataFrame, MAIN_FIELDS = MAIN_FIELDS, SUPER_FIELDS = SUPER_FIELDS, PLOT_MATH = TRUE, PAD_LEFT = TRUE)
  BridgesDataFrame <- BridgesDataFrame[order(BridgesDataFrame$FAIL_CAUS_CODE,BridgesDataFrame$YR_BLT_EST,BridgesDataFrame$YR_FAIL, decreasing = TRUE, na.last = FALSE),]
  limits <- BridgesDataFrame[,"LABEL"]
  if(nrow(BridgesDataFrame)==35){ # set up for spacing between collapse types if using full data set
    limits <- c(limits[1:5],"a",limits[6],"b",limits[7:22],"c",limits[23:35])
    colorsLab <- c(rep("black",5),"white","black","white",rep("black",16),"white",rep("black",13))
  }
  else{
    colorsLab <- rep("black",nrow(BridgesDataFrame))
  }
  
  
  
  p  <- list()
  nP <- length(plotTypes)
  if (length(LEGEND)!=nP) LEGEND <- rep(LEGEND,nP)
  if (length(LABELS)!=nP) LABELS <- rep(LABELS,nP)
  
  for (i in 1:nP){
    pT     <- plotTypes[i]
    if (Q_units == "cfs"){
      Breaks <- switch(pT,
                       pLogD  = c(600,1000,5000,10000,50000,100000, 122000),
                       pLogIP = c(50,100,500,1000,5000,10000,50000,100000, 500000)
      )
      pmargin <- c(0.16,0,0.22,0)
    }
    else{
      if (Q_units == "m3s"){
        Breaks <- switch(pT,
                         pLogD  = c(1.8, 5, 10, 50, 100, 500, 1000, 3300),
                         pLogIP = c(40,50,100,500,1000,5000,7000)
        )
        pmargin <- c(0.16,0,0,0) # don't need to add extra margin because T_R plot has same max axis tick value (1000)
      }
      else{
        warning('Only cfs and m3s are supported Q_units')
        return()
      }
    }
    
    cols <- switch(pT,
                   pLogD  = c("LABEL","Q_FAIL_D_USGS","Q_MAXPREFAIL_D_USGS","Q_MAX_D_USGS","Q100_D_HECD_USGS","Q500_D_HECD_USGS"),
                   pLogIP = c("LABEL","Q_FAIL_IP_USGS","Q_MAXPREFAIL_P_USGS","Q_MAX_P_USGS","Q100_HEC_D","Q500_HECP_USGS")
    )
    title <- switch(pT,
                    pLogD  = "USGS DAILY MEAN FLOW",
                    pLogIP = "USGS INST. FLOW"
    )
    
    ylab <- switch(Q_units,
                   cfs = "Q [1000 cu ft/s]",
                   m3s = expression(paste("Q [",m^{3},"/s]"))
    )
    
    if(Q_units == "cfs") BreaksLabels <- as.character(Breaks/1000) 
    else BreaksLabels <- as.character(Breaks)

    df.subset <- BridgesDataFrame[,cols]
    colnames(df.subset)[2:6] <- c("FAIL", "PREFMAX", "MAX", "Q100", "Q500")
    
    if (Q_units == "m3s"){
      cfs2m3s <- 0.0283168
      Q_cols <- c("FAIL", "PREFMAX", "MAX", "Q100", "Q500")
      df.subset[,Q_cols] <- sapply(Q_cols, function(j) df.subset[,j]*cfs2m3s)
    }
    
    if (!any(is.na(ONLY)) & !any(is.null(ONLY))){
      colsOnly <- "LABEL"
      for(only in ONLY){
        colsOnly <- c(colsOnly, colnames(df.subset)[grepl(only,colnames(df.subset))])
      }
      df.subset <- BridgesDataFrame[,colsOnly]
    }
    
    TYPES     <- factor(colnames(df.subset)[2:ncol(df.subset)], levels = c("FAIL", "PREFMAX", "MAX", "Q100", "Q500"), labels = labelsP$Qtype[1:5])
    shapes <- shapesP$Qtype[TYPES]
    sizes  <- sizesP$Qtype[TYPES]
    
    df.melt   <- melt(df.subset, 
                      id.names = "LABEL", value.name = "Q", variable.name = "GROUP")
    df.melt       <- join(df.melt, BridgesDataFrame[,c("LABEL", "FAIL_CAUS_CODE", "HURRICANE", "BOOL_KNOWN_FAIL_DATE")], by = "LABEL")
    df.melt$TYPE  <- factor(df.melt$GROUP, levels = c("FAIL", "PREFMAX", "MAX","Q100","Q500"), labels = labelsP$Qtype[1:5])
    df.line       <- df.subset
    df.melt2      <- join(df.melt, df.line, by = "LABEL", match = "first")
    df.melt2$FAIL[df.melt2$FAIL < Breaks[1]] <- Breaks[1] + 1 
    df.melt2$Q100[df.melt2$Q100 > Breaks[length(Breaks)]] <- Breaks[length(Breaks)] - 1
    df.melt2$Q500[df.melt2$Q500 > Breaks[length(Breaks)]] <- Breaks[length(Breaks)] - 1
    df.melt2$MAX[df.melt2$MAX > Breaks[length(Breaks)]] <- Breaks[length(Breaks)] - 1
    df.melt2$PREFMAX[df.melt2$PREFMAX > Breaks[length(Breaks)]] <- Breaks[length(Breaks)] - 1
    # return(df.melt2)
    
    # make plot
    p[[pT]] <- ggplot(data=df.melt2, aes(x = LABEL, y = Q))
    p[[pT]] <- p[[pT]] + 
      geom_point(aes(shape = TYPE, size = TYPE, color = FAIL_CAUS_CODE, alpha = BOOL_KNOWN_FAIL_DATE), na.rm = TRUE)
    # return(p)
    if (SEGMENTS){
      if(all(c("MAX","PRE-C MAX") %in% as.character(TYPES))){
        p[[pT]] <- p[[pT]] + 
          geom_linerange(data = subset(df.melt2, GROUP == "FAIL"),aes(ymin = PREFMAX, ymax = MAX, color = FAIL_CAUS_CODE, alpha = BOOL_KNOWN_FAIL_DATE), size = 0.25, lty="dashed", na.rm = TRUE) 
      }
      if(all(c("COLLAPSE","PRE-C MAX") %in% as.character(TYPES))){
        p[[pT]] <- p[[pT]] + 
          geom_linerange(data = subset(df.melt2, GROUP == "FAIL"),aes(ymin = FAIL, ymax = PREFMAX, color = FAIL_CAUS_CODE, alpha = BOOL_KNOWN_FAIL_DATE), size = 0.25, lty = "dotted", na.rm = TRUE)
      } 
      if(all(c("Q100","Q500") %in% as.character(TYPES))){
        p[[pT]] <- p[[pT]] + 
          geom_linerange(data = subset(df.melt2, GROUP == "FAIL"),aes(ymin = Q100, ymax = Q500, color = FAIL_CAUS_CODE, alpha = BOOL_KNOWN_FAIL_DATE), size = 0.25, na.rm = TRUE) 
      }
    }
    
    p[[pT]] <- p[[pT]] + 
      coord_flip() +  labs(y = ylab, title = title) +
      scale_x_discrete(limits = factor(limits, levels = limits, labels = limits), labels = as.expression(parse(text=levels(factor(limits, levels = limits, labels = limits))))) +
      scale_shape_manual(values = shapes,    name = legendsP$Qtype) +
      scale_color_manual(values = colorsP$Fail,     name = legendsP$Fail) +
      scale_size_manual(values = sizes) +
      scale_alpha_manual(values = alphasP$FailDate, name = legendsP$Date)
    
    if(Q_units=="cfs"){
      p[[pT]] <- p[[pT]] + 
        scale_y_log10(limits = c(Breaks[1],Breaks[length(Breaks)]), breaks = Breaks[2:(length(Breaks)-1)], minor_breaks = NULL, labels = BreaksLabels[2:(length(Breaks)-1)])
    }
    else{ # m3/s
      p[[pT]] <- p[[pT]] + scale_y_log10(breaks = c(5, 10, 50, 100, 500, 1000))
    }
    
    if(LEGEND[i] == TRUE){
      p[[pT]] <- p[[pT]] +  guides(shape = guide_legend(order = 1,
                                                        override.aes = list(size = sizes)),
                                   color = guide_legend(order = 2,
                                                        override.aes = list(shape=15,
                                                                            size = 4,
                                                                            linetype = "blank")),
                                   alpha = guide_legend(order = 3,
                                                        override.aes = list(shape = 15,
                                                                            size  = 4,
                                                                            linetype = "blank")),
                                   size  = FALSE,
                                   fill  = FALSE,
                                   group = FALSE
      ) 
    }
    else{
      p[[pT]] <- p[[pT]] + guides(shape = FALSE,
                                  color = FALSE,
                                  alpha = FALSE,
                                  size  = FALSE,
                                  fill  = FALSE
      ) 
    }
    
    p[[pT]] <- p[[pT]] + getTheme(outputType) +
      theme(axis.text.x  = element_text(color = "black", size = textP$reg[outputType], angle = 90, vjust = 0.5, hjust = 1, 
                                        margin  = margin(pmargin[1],pmargin[2],pmargin[3],pmargin[4], "cm")),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            legend.margin = margin(t=-0.1,r=-0.1,b=-0.1,l=-0.1, "cm"),
            plot.margin   = unit(c(0.1,0,0,0), "lines"))
            
    if (LABELS[i] == TRUE){
      p[[pT]] <- p[[pT]] +
        theme(axis.text.y  = element_text(color = colorsLab,size = textP$reg[outputType], hjust = 0, vjust = 0)
        )
    }
    else{
      p[[pT]] <- p[[pT]] +
        theme(axis.text.y  = element_blank()
        )
    }
  }
  
  # GRID ARRANGE
  grp <- lapply(p, function(i) ggplot_gtable(ggplot_build(i)))
  if (SAVE) MakeGridPlot(grp, SCREEN = FALSE, SAVE = SAVE, SIZE = SIZE, SAVENAME = "Fig4a", embedFonts = embedFonts, gs_path = gs_path)
  return(p)
  
}
