# Figure 1 in "Historical analysis of hydraulic bridge collapses in the continental United States"
# Map of US with locations of all bridges over rivers, differentiated for pre- or post-1991 design
# BridgesDataFrame should include only rows to be plotted
# ggmap controls basemap used -- ggmap or plain outline of states
# type should be one of {"1991", "ageContinuous", "ageDiscrete"}
# Copyright Madeleine Flint, 2016

PlotAllBridgesMap <- function(BridgesDataFrame, ggmap = FALSE, type = "ageDiscrete", SAVE = FALSE, 
                              SIZE = c(3.6,1.9), outputType = "PRINT", outputFormat = "pdf",
                              embedFonts = TRUE, gs_path = "/usr/local/bin/gs"){
  if(ggmap){
    stop('ggmap is no longer supported by this function.')
  }
  require(rgdal)
  require(grid)
  require(gridExtra)
  require(plyr)
  require(reshape2)
  require(scales)
  if(!("colorsP" %in% ls(.GlobalEnv))) source(file.path("Scripts","Plotting","SetupEncoding.R"))
  if(!("getTheme" %in% ls(.GlobalEnv))) source(file.path("Scripts","Plotting","getTheme.R"))
  
  # SET UP ENCODINGS AS FACTORS
BridgesDataFrame$ITEM27[BridgesDataFrame$ITEM27 < 1697] <- NA
  ageBreaks   <- c(1697, 1851, 1901, 1951, 1991, 2001, 2011, 2019)
  ageLabels   <- c("< 1851", "1851-1901", "1901-1951", "1951-1991","1991-2001",  "2001-2011", "> 2011")
  BridgesDataFrame$lat   <- BridgesDataFrame$LATDD
  BridgesDataFrame$lon  <- BridgesDataFrame$LONGDD

  # SET UP BASEMAP AND PLOT - GGMAP OPTION
  if (ggmap){
    basemap <- get_map(location="United States",zoom=3,maptype="road",source="google",color="bw")
    map <- ggmap(basemap, extent="normal", legend="bottom")+
                 ylim(24,50) + xlim(-125,-66)
  }
  else{
    BridgesDataFrame$group <- factor(2)
    load(file.path("Data","df.USstates.RData"))
    colnames(df.USstates)[1] <- "lon"
    map <- ggplot(df.USstates, aes(x = lon, y = lat, group = group)) + geom_polygon(color = "gray", fill = NA, size = 0.25) + 
      coord_fixed(ratio = 1, xlim = c(-125,-66), ylim = c(24,50))
  }

  if (type == "1991"){
    BridgesDataFrame$DESIGN_CATEGORY <- factor(BridgesDataFrame$ITEM27>=1991, levels = c("TRUE","FALSE"), labels = c("POST-1991", "PRE-1991"), exclude = NA)
    BridgesDataFrame <- BridgesDataFrame[!is.na(BridgesDataFrame$DESIGN_CATEGORY),]
    map <- map + 
      geom_point(data = BridgesDataFrame,
                 aes(color = DESIGN_CATEGORY), 
                 shape = shapesP$BridgeDesign[1],
                 size  = 0.5,
                 alpha = alphasP$BridgeDesign[1],
                 stroke = 0,
                 na.rm = TRUE)  +
      scale_color_manual(values = colorsP$BridgeDesign, breaks = c("POST-1991", "PRE-1991"),  name = "DESIGN YEAR")
  }
  
  if (type == "ageContinuous"){
    BridgesDataFrame <- BridgesDataFrame[!is.na(BridgesDataFrame$ITEM27),]
    map <- map + 
      geom_point(data = BridgesDataFrame,
                 aes(color = ITEM27), 
                 shape = shapesP$BridgeDesign[1],
                 size  = 0.5,
                 alpha = alphasP$BridgeDesign[1],
                 stroke = 0,
                 na.rm = TRUE)  +
      scale_color_gradientn(colours = colorsP$BridgeAge9, values = rescale(ageBreaks), breaks = ageBreaks, name = "YEAR BUILT")
  }

  if (type == "ageDiscrete"){ 
    BridgesDataFrame$AGE_CATEGORY <- unlist(sapply(1:nrow(BridgesDataFrame), 
                                                   function(i) ageBreaks[BridgesDataFrame[i,"ITEM27"] <= ageBreaks[2:8] & BridgesDataFrame[i,"ITEM27"] >= ageBreaks[c(1:(length(ageBreaks)-1))]][1]))
    BridgesDataFrame <- BridgesDataFrame[!is.na(BridgesDataFrame$AGE_CATEGORY),]
    BridgesDataFrame$AGE_CATEGORY <- factor(BridgesDataFrame$AGE_CATEGORY, levels = c(1697,1851,1901,1951,1991,2001,2011), labels = ageLabels, exclude = NA)
    map <- map + 
      geom_point(data = BridgesDataFrame,
                 aes(color = AGE_CATEGORY), 
                 shape = shapesP$BridgeDesign[1],
                 size  = 0.5,
                 alpha = alphasP$BridgeDesign[1],
                 stroke = 0,
                 na.rm = TRUE)  +
      scale_color_manual(values = colorsP$BridgeAge9[c(1:4,7:9)],  name = "") # breaks = ageBreaks, labels = ageLabels,
  }
  
  if(grepl("age",type)){
    map <- map + guides(color = guide_legend(nrow = 2,
                                             byrow= TRUE,
                                             override.aes = list(size = 4)))
  }
  else{
    map <- map + guides(color = guide_legend(override.aes = list(size = 4)))
  }
  
  map <- map + getTheme(outputType, TRUE)
  map <- map +
    theme(panel.spacing = unit(c(-0.25,-0.25,-0.25,-0.25),"in"),
          legend.key.height=unit(0.4,"line"),
          legend.key.width=unit(0.5,"line"),
          legend.position = "bottom",
          legend.margin = margin(-0.1,-0.1,0,-0.1,"in"),
          plot.margin     = unit(c(-0.1,-0.1,0,-0.1), "in"))
  if (SAVE==TRUE){
    if (outputFormat == "pdf"){
      pdf(file=file.path("Plots","Fig1.pdf"), width=SIZE[1],height=SIZE[2], title = "Fig 1 Map of US Bridges Over Water",
          useDingbats = F)
      print(map)
      dev.off()
    }
    else{
      if (outputFormat == "png"){
        ggsave(map,file=file.path("Plots","Fig1.png"), width=SIZE[1],height=SIZE[2])
      }
      else warning('OutputFormat must be pdf or png')
    }

    if (embedFonts & outputFormat == "pdf"){
      require(extrafont)
      Sys.setenv(R_GSCMD=gs_path) #path to ghostscript
      embed_fonts(file.path("Plots",paste("BridgesMap",type,"pdf",sep="."))) 
    } 
  }
  return(map)
}
