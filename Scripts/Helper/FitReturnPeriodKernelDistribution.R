FitReturnPeriodKernelDistribution <- function(BridgesDataFrame){
require(Hmisc)

# analyze return period of failure events using mean and median data
TfMeanD <- mean(BridgesDataFrame$T_FAIL_D_HECD_USGS)
TfMeanIP <- mean(BridgesDataFrame$T_FAIL_IP_HECP_USGS,na.rm=TRUE)
TfMedianD <- median(BridgesDataFrame$T_FAIL_D_HECD_USGS)
TfMedianIP <- median(BridgesDataFrame$T_FAIL_IP_HECP_USGS,na.rm=TRUE)

# using kernel density for T and convolution approach
pTkernelD  <- density(BridgesDataFrame$T_FAIL_D_HECD_USGS, bw = "nrd", adjust = 0.5, from=0, to=2000)
pTkernelIP <- density(BridgesDataFrame$T_FAIL_IP_HECP_USGS[!is.na(BridgesDataFrame$T_FAIL_IP_HECP_USGS)], bw = "nrd",adjust = 0.5, from=0, to=11000)
pTkernelDinterp <- approxExtrap(pTkernelD$x, pTkernelD$y, t)
pTkernelIPinterp <- approxExtrap(pTkernelIP$x, pTkernelIP$y, t)

return()
}