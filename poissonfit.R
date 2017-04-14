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

library(data.table)
library(fitdistrplus)
library(ggplot2)
library(foreach)

data <- fread("onepoint.txt")
setnames(data, "V1", "x")

ggplot(data, aes(x=x)) + geom_bar()

poissonFit <- poissonFit <- fitdist(x, "pois")
ppois(7, poissonFit$estimate)

data[, mean(x)/var(x)]

bootstrapDispersion <- foreach(i = 1 : 100, .combine = "c" ) %do% {
  tmp <- rpois(n=nrow(data), lambda = poissonFit$estimate)
  mean(tmp) / var(tmp)
}

# mod.null$fitted.values should be the same as poissonFit$estimate

negbinFit <- fitdist(x, "nbinom")

