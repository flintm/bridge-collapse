# Performs correlation of Q and T estimates related to failure and collapse
# Copyright Madeleine Flint, 2016
# modified to combine failure and collapse by Madeleine Flint, 2018-06-22

CorrelationsFailMaxEvents <- function(BridgesDataFrame = NULL,TYPES = c("FAILQ", "USGS","MAXQ", "USGS-TMAX"),
                                  SAVE = TRUE, VERBOSE = FALSE){
  
source(file.path("Scripts","Helper","Bases.R"))
  
if(is.null(BridgesDataFrame)){ 
  load(file.path("Data","df.Fail.NBI.Gage.RData"))
  BridgesDataFrame <- df.Fail.NBI.Gage
  rm(df.Fail.NBI.Gage)
}

require(stats)
require(Kendall)
require(SuppDists)
require(pspearman)
require(nsRFA)

kgCfs          <- 0.035315*(1/3600)

ls.corrs <- list()
for (type in TYPES){
  if (VERBOSE) print(type)
  ls.corrs[[type]] <- list()
  ls.Bases       <- Bases(type)
  for (i in 1:length(ls.Bases$bases)){
    T1 <- BridgesDataFrame[rowsToView,ls.Bases$T1ls[i]]
    T2 <- BridgesDataFrame[rowsToView,ls.Bases$T2ls[i]]
    ls.corrs[[type]][[paste(ls.Bases$bases[i],"P",sep="")]]      <- cor.test(T1,T2,
                                                                                  method="pearson",
                                                                                  conf.level=0.95,
                                                                                  exact=TRUE)
    ls.corrs[[type]][[paste(ls.Bases$bases[i],"K",sep="")]]       <- cor.test(T1,T2,
                                                                                   method="kendall",
                                                                                   conf.level=0.95,
                                                                                   exact=TRUE)
    ls.corrs[[type]][[paste(ls.Bases$bases[i],"Papprox",sep="")]] <- cor.test(T1,T2,
                                                                                   method="pearson",
                                                                                   conf.level=0.95,
                                                                                   exact=FALSE)
    ls.corrs[[type]][[paste(ls.Bases$bases[i],"Kapprox",sep="")]] <- cor.test(T1,T2,
                                                                                   method="kendall",
                                                                                   conf.level=0.95,
                                                                                   exact=FALSE)
    ls.corrs[[type]][[paste(ls.Bases$bases[i],"LR",sep="")]]      <- lm(T2 ~ T1)
  }
}


## SAVE ############ ------
if (SAVE){
  savefile <- paste(gsub("-","",Sys.Date()),"CorrelationsOfFailMaxEvents.RData", sep="_")
  save(ls.corrs, file = file.path("Data",savefile))
}
return(ls.corrs)
}
