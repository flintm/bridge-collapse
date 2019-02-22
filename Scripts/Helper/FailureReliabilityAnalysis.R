FailureReliabilityAnalysis <- function(shift = 0.1, SAVE = FALSE)
{
require(Hmisc)
load(file.path("Data","df.Fail.NBI.Gage.RData"))

# analyze return period of failure events using mean and median data
TfMeanD <- mean(df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_HECD_USGS"])
TfMeanIP <- mean(df.Fail.NBI.Gage[rowsToView,"T_FAIL_IP_HECP_USGS"],na.rm=TRUE)
TfMedianD <- median(df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_HECD_USGS"])
TfMedianIP <- median(df.Fail.NBI.Gage[rowsToView,"T_FAIL_IP_HECP_USGS"],na.rm=TRUE)

# Final annual probability of failure with all mass at median
pMeanD   <- 1/TfMeanD
pMeanIP  <- 1/TfMeanIP
pMedianD   <- 1/TfMedianD
pMedianIP  <- 1/TfMedianIP

# lognormal distribution for return period of failure event
logTfD  <- log(df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_HECD_USGS"])
logTfIP <- df.Fail.NBI.Gage[rowsToView,"T_FAIL_IP_HECP_USGS"]
logTfIP <- logTfIP[!is.na(logTfIP)]
meanlogTfD <- mean(logTfD)
sdlogTfD   <- sd(logTfD)
meanlogTfIP <- mean(logTfIP)
sdlogTfIP   <- sd(logTfIP)

# to get probability of failure 
# convolution integral of probability that it fails given the return period is "t"
# times the probability of seeing an event with probability "t"
dt <- 1
t <- seq(dt,12000,by=dt)

pFlogD <- 0
pFlogIP <- 0
for (i in 1:length(t)){
  dP <- dnorm(log(t[i]),meanlogTfD,sdlogTfD)*1/t[i]^2
  pFlogD <- pFlogD + dP
  dP <- dnorm(log(t[i]),meanlogTfIP,sdlogTfIP)*1/t[i]^2
  pFlogIP <- pFlogIP + dP
}

# using kernel density for T and convolution approach
pTkernelD  <- density(df.Fail.NBI.Gage[rowsToView,"T_FAIL_D_HECD_USGS"], bw = "nrd", adjust = 0.5, from=0, to=2000)
pTkernelIP <- density(df.Fail.NBI.Gage[rowsToView,"T_FAIL_IP_HECP_USGS"][!is.na(df.Fail.NBI.Gage[rowsToView,"T_FAIL_IP_HECP_USGS"])], bw = "nrd",adjust = 0.5, from=0, to=11000)
pTkernelDinterp <- approxExtrap(pTkernelD$x, pTkernelD$y, t)
pTkernelIPinterp <- approxExtrap(pTkernelIP$x, pTkernelIP$y, t)
pFkernelD  <- 0
pFkernelIP <- 0
for (i in 1:length(t)){
  dP <- pTkernelDinterp$y[i]*1/t[i]^2
  pFkernelD <- pFkernelD + dP
  dP <- pTkernelIPinterp$y[i]*1/t[i]^2
  pFkernelIP <- pFkernelIP + dP
}

# considering potential impact of climate change (uniform shift in flood hazard)
pFkernelDp  <- 0
pFkernelIPp <- 0
for (i in 1:length(t)){
  dP <- pTkernelDinterp$y[i]*1/(t[i]*(1-shift))^2
  pFkernelDp <- pFkernelDp + dP
  dP <- pTkernelIPinterp$y[i]*1/(t[i]*(1-shift))^2
  pFkernelIPp <- pFkernelIPp + dP
}

pFkernelDl  <- 0
pFkernelIPl <- 0
for (i in 1:length(t)){
  dP <- pTkernelDinterp$y[i]*1/(t[i]*(1+shift))^2
  pFkernelDl <- pFkernelDl + dP
  dP <- pTkernelIPinterp$y[i]*1/(t[i]*(1+shift))^2
  pFkernelIPl <- pFkernelIPl + dP
}

pMeanDp <- 1/(TfMeanD*(1-shift))
pMeanIPp <- 1/(TfMeanIP*(1-shift))
pMeanDl <- 1/(TfMeanD*(1+shift))
pMeanIPl <- 1/(TfMeanIP*(1+shift))
pMedianDp <- 1/(TfMedianD*(1-shift))
pMedianIPp <- 1/(TfMedianIP*(1-shift))
pMedianDl <- 1/(TfMedianD*(1+shift))
pMedianIPl <- 1/(TfMedianIP*(1+shift))

# theoretical reliability
pF35 <- pnorm(-3.5)
pF50y175 <- (1/50)*pnorm(-1.75)
pF100y175 <- (1/100)*pnorm(-1.75)
pF50y175.p <- (1/50/(1-shift))*pnorm(-1.75)
pF100y175.p <- (1/100/(1-shift))*pnorm(-1.75)
pF50y175.l <- (1/50/(1+shift))*pnorm(-1.75)
pF100y175.l <- (1/100/(1+shift))*pnorm(-1.75)

# organize and save data
pTdata <- list(d   = c(mean = TfMeanD, median = TfMedianD),
               ip  = c(mean = TfMeanIP, median = TfMedianIP))
pFannualEstimates <- list(pFD  = c(mean = pMeanD, median = pMedianD, logn = pFlogD, kernel = pFkernelD),
                          pFIP = c(mean = pMeanIP, median = pMedianIP, logn = pFlogIP, kernel = pFkernelIP),
                          pFrel = c("3pt5" = pF35, "50y1pt75" = pF50y175, "100yr1pt75" = pF100y175),
                          pFDp = c(mean = pMeanDp, median = pMedianDp, kernel = pFkernelDp),
                          pFIPp = c(mean = pMeanIPp, median = pMedianIPp, kernel = pFkernelIPp),
                          pFrelp = c("3pt5" = NA, "50y1pt75" = pF50y175.p, "100yr1pt75" = pF100y175.p),
                          pFDl = c(mean = pMeanDl, median = pMedianDl, kernel = pFkernelDl),
                          pFIPl = c(mean = pMeanIPl, median = pMedianIPl, kernel = pFkernelIPl),
                          pFrell = c("3pt5" = NA, "50y1pt75" = pF50y175.l, "100yr1pt75" = pF100y175.l))
if(SAVE) save(pFannualEstimates, pTdata, file = file.path("Data", "FailureReliabilityComparison.RData"))
return(pFannualEstimates)
}