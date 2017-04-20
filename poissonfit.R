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

poissonFit <- fitdist(data$x, "pois")
ppois(7, poissonFit$estimate)

data[, mean(x)/var(x)]

bootstrapDispersion <- foreach(i = 1 : 1000, .combine = "c" ) %do% {
  tmp <- rpois(n=nrow(data), lambda = poissonFit$estimate)
  mean(tmp) / var(tmp)
}

# mod.null$fitted.values should be the same as poissonFit$estimate

negbinFit <- fitdist(x, "nbinom")

# Where is the data coming from (lat, lon)?
# Is there some persistency in the underlying daily precip data, e.g. 
# consequtive exceedances of 20mm? (For NL 4 days on averages seems quite high.)

# over dispersion -> maybe binomial distribution
nDaysSeason <- 30+31+30+31+31+30
binFit <- fitdist(data$x, "binom", fix.arg=list(size=nDaysSeason), start=list(prob=0.01))




plot(seq(0, 0.99, by = 0.01), qbinom(seq(0, 0.99, by = 0.01), size = nDaysSeason, prob = 0.021))
pbinom(1, size = nDaysSeason, prob = 0.021)

1 - pbinom(6, size = nDaysSeason, prob = 0.021)
pbinom(1, size = nDaysSeason, prob = 0.021)
1 - pbinom(7, size = nDaysSeason, prob = 0.021)


## continuous distribution with grouped likelihood
# normFit <- fitdist(data$x, "norm")

# beta?
