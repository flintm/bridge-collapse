# Set up encoding for plots and tables for historical bridge failures paper
# Copyright Madeleine Flint, 2016

# COLOR AND FILL PALETTES AND ALPHAS (TRANSPARENCY)
colorsP <- list()
fillsP  <- list()
alphasP <- list()
# SHAPES AND SHAPE SIZES
shapesP       <- list()
sizesP        <- list()
# SETUP LISTS OF LINE TYPES AND WIDTHS 
linesP       <- list()
lwP          <- list()
# SETUP LISTS OF LEVELS AND LABELS (AXES, LEGENDS)
labelsP   <- list()
levelsP   <- list()
legendsP  <- list()
# SETUP LIST OF TEXT SIZE 
textP <- list()

# FAILURE CAUSES, DATES, HURRICANES, DEGREE OF REGULATION, AND RATIOS OF DRAINAGE AREA ----------
# Failure cause
# red/blue/green/purple/orange intense 
#e41a1c
#377eb8
#4daf4a
#984ea3
##ff7f00
legendsP$Fail  <- "FAILURE CAUSE"
labelsP$Fail <- c("FLOOD","SCOUR","HURRICANE","OTHER")
colorsP$Fail <-  c(FLOOD = "#377eb8", SCOUR = "#e41a1c", HURRICANE = "#984ea3", OTHER = "#ff7f00")
fillsP$Fail  <- colorsP$Fail
# Failure date
legendsP$Date  <- "FAIL DATE"
labelsP$Date <- c("KNOWN","UNKNOWN")
alphasP$FailDate <- c(KNOWN = 1, UNKNOWN = 0.2)
# Hurricane
labelsP$Hurr <- c("YES","N.A.")
colorsP$Hurr <- c(YES = "#984ea3", N.A. = NA)
fillsP$Hurr  <- colorsP$Hurr
alphasP$Hurr <- c(HURRICANE = 1, NOH = 0)
# Regulation status
legendsP$Reg   <- "FLOW ALTERED"
labelsP$Reg   <- c("REGULATED", "UNREGULATED", "UNKNOWN")
alphasP$Reg  <- c(REGULATED = 0.3, UNGREGULATED = 1, UNKNOWN = 0.1)
colorsP$Reg  <- c(FAILURE="white",MAX="black",PARTIAL="white")
shapesP$Reg  <- c(REGULATED="R", UNREGULATED="U", UNKNOWN="N")
sizesP$Reg   <- c(PRINT = 2, PRES = 4)
# From instantaneous or peak data
alphasP$INST <- c(0.3, 0.9)
legendsP$INST <- "DAILY OR INST/PK DATA"


# POINT TYPE (FAILURE, MAX, ETC), INCL. CORRELATION PLOTS --------
legendsP$Qtype <- "DATA"
labelsP$Qtype <- c("FAILURE", "PRE-FAIL MAX", "MAX", "Q100", "Q500","PARTIAL")
shapesP$Qtype  <- c(FAILURE = 19, "PRE-FAIL MAX" = 1, MAX = 21,    "Q100" = 5, "Q500" = 5,"FAIL REPORT" = 73, PARTIAL=19)
sizesP$Qtype   <- 2*c(FAILURE = 0.4, "PRE-FAIL MAX" = 1, MAX = 1.6,  "Q100" = 1, "Q500" = 1.4, "FAIL REPORT" = 1.2, PARTIAL=0.4)
sizesP$Failure    <- 5
shapesP$Failure <- 19 
levelsP$Corrs  <- c("Q_FAIL_D_USGS",     "Q_FAIL_I_USGS", "Q_FAIL_P_USGS", "Q_FAIL_IP_USGS","Q_FAILPM2_DVICG",
                    "T_FAIL_D_HECD_USGS", "T_FAIL_I_HECP_USGS", "T_FAIL_P_HECP_USGS", "T_FAIL_IP_HECP_USGS",
                    "T_FAIL_D_PARTDUR_USGS","T_FAIL_IP_PARTDUR_USGS",
                    "Q_MAX_D_USGS","Q_MAX_P_USGS",
                    "T_MAX_D_HECD_USGS",  "T_MAX_P_HECP_USGS")
labelsP$Corrs  <- c("USGS FAIL DAILY MEAN [cfs]",  "USGS FAIL INST. [cfs]", "USGS FAIL PEAK [cfs]", "USGS FAIL INST/PEAK [cfs]",
                    "USGS FAIL DAILY BULL. 17B [yr]","USGS FAIL INST BULL. 17B [yr]","USGS FAIL PEAK BULL. 17B [yr]", "USGS FAIL I/P BULL. 17B [yr]", 
                    "USGS FAIL DAILY PARTIAL DUR.", "USGS FAIL I/P PARTIAL DUR.",
                    "USGS MAX DAILY MEAN [cfs]", "USGS MAX PEAK [cfs]", 
                    "USGS MAX DAILY MEAN BULL. 17B [yr]", "USGS-MAX PEAK BULL. 17B [yr]" )
sizesP$FailCorr   <- c(PRINT = 4, PRES = 8)

# HYDROLOGICAL DATA TYPES --------------
# teal/orange/purple/pink - data (maybe just use teal and orange)
#1b9e77
#d95f02
#7570b3
#e7298a
legendsP$Data  <- "DATA"
labelsP$Data   <- c("USGS","USGS")
levelsP$Data   <- c("USGS","HEC")
colorsP$Data <- c(USGS = "#1b9e77")
fillsP$Data  <- colorsP$Data
linesP$Data  <- c(USGS = "solid")
lwP$Data     <- c(USGS = 2)

# BRIDGE AGE/DESIGN -----------
# for age of bridges - 9 from red -> yellow -> blue
#d73027
#f46d43
#fdae61
#fee090
#ffffbf
#e0f3f8
#abd9e9
#74add1
#4575b4
colorsP$BridgeAge9    <- c("#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf", 
                           "#e0f3f8", "#abd9e9", "#74add1", "#4575b4")
alphasP$BridgeDesign  <- c("PRE-1991" = 0.4, "POST-1991" = 0.4)
colorsP$BridgeDesign  <- c("PRE-1991" = colorsP$BridgeAge9[1], "POST-1991" = colorsP$BridgeAge9[9])
shapesP$BridgeDesign <- c(PRE91 = 20, POST91 = 20)
sizesP$BridgeDesign  <- c(PRE91 = 1,  POST91 = 1)

# MATERIAL AND TYPES OF BRIDGES
colorsP$MatTypes <- c("0"='#a6cee3', "1"='#1f78b4',"2"='#b2df8a',"3"='#33a02c',"4"='#fdbf6f',"5"='#ff7f00',"6"='#cab2d6',"7"='#6a3d9a')
levelsP$Mat      <- as.character(c(1,3,5,7,8,9,0))
labelsP$Mat      <- c("1" = "concrete",
                      "3" = "steel",
                      "5" = "prestressed concrete",
                      "7" = "wood",
                      "8" = "masonry",
                      "9" = "metal",
                      "0" = "other")
names(colorsP$MatTypes) <- labelsP$Mat
levelsP$Type     <- as.character(c(1:17,19:22,0))
labelsP$Type     <- c("1" = "slab", 
                      "2" = "stringer", 
                      "3" = "floorbeam",
                      "4" = "t beam", 
                      "5" = "box multiple", 
                      "6" = "box single",
                      "7" = "frame", 
                      "8" = "orthotropic", 
                      "9" = "truss deck",
                      "10" = "truss thru", 
                      "11" = "arch deck", 
                      "12" = "arch thru",
                      "13" = "suspension", 
                      "14" = "stayed girder", 
                      "15" = "lift",
                      "16" = "bascule", 
                      "17" = "swing", 
                      "19" = "culvert", 
                      "20" = "approach", 
                      "21" = "segmental box",
                      "22" = "channel beam", 
                      "0" = "other")
legendsP$Mat <- "MATERIAL"
# MISCELLANEOUS FOR PLOTS ------------
# Background grays
# fillsP$grays          <- gray(seq(0.3,0.8,length.out = 3))
fillsP$grays <- c("white",gray(seq(0.9,0.8,length.out=2)))
alphasP$grays          <- 0.3
# Significance for trend plot
alphasP$MK   <- c("FALSE" = 0.4, "TRUE" = 0.9)
colorsP$MK   <- c("POS" = colorsP$BridgeAge9[9], "NEG" = colorsP$BridgeAge9[1])
labelsP$MK   <- c("POS","NEG")
# Annotation and header text size
textP$head <- c(PRINT = 8, PRES = 16)
textP$reg  <- c(PRINT = 8, PRES = 12)
textP$sub  <- c(PRINT = 8, PRES = 14)
textP$labels <- c(PRINT = 8, PRES = 12)
textP$labelcex <- 0.7
textP$annotate <- c(PRINT = 2.8, PRES = 6)