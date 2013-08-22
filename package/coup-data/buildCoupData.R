# Build coup demo data
# Andreas Beger
# 22 August 2013

setwd("~/Work/spdur_package/package/coup-data")

# Create country-year panel base ------------------------------------------

library(cshapes)

years <- seq(1979, 2010, 1)
date <- paste(years, '-6-30', sep='')

gwPanel <- NULL
for(i in seq_along(date)) {
  gwcodesYear <- cshp(date=as.Date(date[i]), useGW=TRUE)$GWCODE
  gwPanel <- rbind(gwPanel, cbind(gwcode=gwcodesYear, year=years[i])) 
}
gwPanel <- as.data.frame(gwPanel)


# P&T Coups ---------------------------------------------------------------

# from

pt <- read.table("powell_thyne_ccode_year.txt", sep="\t", header=T)
pt <- subset(pt, year>=1979, select=c("ccode", "year", "coup1"))
colnames(pt)[1] <- "gwcode"

library(plyr)

coups <- join(gwPanel, pt, by=c("gwcode", "year"), type="left")
coups$succ.coup <- ifelse(coups$coup1==2, 1, 0)
coups$succ.coup[is.na(coups$succ.coup)] <- 0


# Polity ------------------------------------------------------------------

# from http://www.systemicpeace.org/inscr/inscr.htm

library(gdata)

polity <- read.xls("p4v2012.xls")
polity$gwcode <- polity$ccode
keep.vars <- c("gwcode", "year", "democ", "autoc", "polity", "polity2", 
               "regtrans")
polity <- subset(polity, year>=1979, select=keep.vars)

coups <- join(coups, polity, by=c("gwcode", "year"), type="left")


# Done, save --------------------------------------------------------------

coups$year <- as.Date(paste0(coups$year, "-06-30"))
save(coups, file="coups.rda")

coups.dur <- buildDuration(coups, "succ.coup", "gwcode", "year", freq="yearly")
coups.dur <- coups.dur[!is.na(coups.dur$polity2), ]
model <- spdur(duration ~ polity2, atrisk ~ polity2, data=coups.dur)
summary(model)