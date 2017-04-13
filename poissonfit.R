library(extRemes); library(ncdf4); library(fields)
library(ismev)
#ncr20 <- nc_open("r20mm_AMJJAS_1950-2016.nc")
ncr20 <- nc_open("r20mm_max_AMJJAS_1991-2016.nc")
var <- ncvar_get(ncr20, "r20mmETCCDI")
ilon <- ncvar_get(ncr20, "longitude")
ilat <- ncvar_get(ncr20, "latitude")

xpoint <- var[209,90,]

#' Null model - i.e. data is poisson distributed, 
#' independent of variables. 
#'offset = NULL as assumed dispersion coefficient =1
mod.null <- glm(xpoint~1,family = "poisson", offset = NULL)
mod.null$aic
mod.null$coefficients
