# Names and variable names for correlation and linear regression
# Copyright Madeleine Flint, 2016
Bases <- function(TYPE){
 # FAILURE RELATED ------------------------------------------------------------
  if (TYPE == "FAILQ"){
    bases <- c("USGSdUSGSp",                 "USGSdUSGSi",                 "USGSdUSGSip")
    T1ls  <- c("Q_FAIL_D_USGS",              "Q_FAIL_D_USGS",              "Q_FAIL_D_USGS")
    T2ls  <- c("Q_FAIL_P_USGS",              "Q_FAIL_I_USGS",              "Q_FAIL_IP_USGS")   
  }
  
  if (TYPE == "USGS"){
    bases <- c("HECdHECi",                 "HECdHECp",               "HECiHECp",                    "HECdHECip",             
               "HECdPartInterpd",          "HECipPartInterpd",       "PartInterpdPartInterpip",         
               "HECdPartInterpip",                                   "HECipPartInterpip")
    T1ls  <- c("T_FAIL_D_HECD_USGS",           "T_FAIL_D_HECD_USGS",         "T_FAIL_I_HECP_USGS",              "T_FAIL_D_HECD_USGS",            
               "T_FAIL_D_HECD_USGS",         "T_FAIL_IP_HECP_USGS",         "T_FAIL_D_PARTDUR_USGS",            
               "T_FAIL_D_HECD_USGS",                                     "T_FAIL_IP_HECP_USGS")
    T2ls  <- c("T_FAIL_I_HECP_USGS",          "T_FAIL_P_HECP_USGS",        "T_FAIL_P_HECP_USGS",               "T_FAIL_IP_HECP_USGS",  
               "T_FAIL_D_PARTDUR_USGS",  "T_FAIL_D_PARTDUR_USGS",         "T_FAIL_IP_PARTDUR_USGS", 
               "T_FAIL_IP_PARTDUR_USGS",                    "T_FAIL_IP_PARTDUR_USGS")
  }
 
 # MAX RELATED ------------------------------------------------------------
 if (TYPE == "MAXQ"){
   bases <- "USGSdUSGSp"
   T1ls  <- "Q_MAX_D_USGS"
   T2ls  <- "Q_MAX_P_USGS"
 } 
 
 if (TYPE == "USGS-TMAX"){
   bases <- "HECdHECp"
   T1ls  <- "T_MAX_D_HECD_USGS"
   T2ls  <- "T_MAX_P_HECP_USGS"
 }
 
 # RETURN ---------------------------
  ls.Bases <- list(bases = bases, T1ls = T1ls, T2ls = T2ls)
  return(ls.Bases)
}
