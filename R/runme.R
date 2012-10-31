##########
# What: Test code for spdur dev
# Date: November 2012
# Who:  Andreas Beger
###########

rm(list=ls())

if(Sys.info()["user"]=="adbeger"){
  path="~/Research/spdur_package"}

# for maps
library(cshapes)
library(classInt)
library(RColorBrewer)

# Survival
library(survival)
library(wicews)

source('R/spdur.R')

##########
# Data
#
##########

## Data
load('delete/dur.coup.RData')

## List of coups and their dates
dur.coup[dur.coup$failure==1, c('country', 'date', 'duration')]

## Prep for regression
# Make sure all variables look somewhat normall distributed
dur.coup$med.polity.l1 <- with(dur.coup, ifelse((polity.l1>=-5 & polity.l1<=7), 1, 0))
dur.coup$low.polity.l1 <- with(dur.coup, ifelse((polity.l1<=7), 1, 0))
dur.coup$ln.ProxElection.l1 <- log(dur.coup$ProxElection.l1+1)
dur.coup$ln.reb.l.count.both.l1 <- log(dur.coup$reb.l.count.both.l1+0.1)
dur.coup$ln.eth.rel.l.count.l1 <- log(dur.coup$eth.rel.l.count.l1 + 1)
dur.coup$faction <- as.numeric(dur.coup$parcomp==3)
dur.coup$ln.dom.cris.i.count.l1 <- log(dur.coup$dom.cris.i.count.l1+1)
dur.coup$ln.cum.d.minor.pos.l1 <- log(dur.coup$cum.d.minor.pos.l1+1)
dur.coup$ln.cum.d.major.neg.l1 <- log(dur.coup$cum.d.major.neg.l1+1)
dur.coup$ln.d.minor.neg.l1 <- log(dur.coup$d.minor.neg.l1+1)

# throw out missing cases and test
dur.coup <- subset(dur.coup, complete.cases(dur.coup$ln.gdppc.l1))
dur.coup$trythis<-ifelse(dur.coup$ Xd.neg.var>0,1,0)

test <- subset(dur.coup, date==as.Date('2011-12-01'))

## Split duration model
spdur.coup <- 
  spdur(duration ~ d.major.l1 + d.minor.l1 + ln.dom.cris.i.count.l1 + ln.ProxElection.l1,
        atrisk ~ ln.gdppc.l1 + med.polity.l1 + ln.dom.cris.i.count.l1 + ln.ProxElection.l1,
        data=dur.coup, test=test, last=dur.coup$end.spell, 
        distr='weibull', iter=200, sims=1000)

# Separation plot
predy.in <- spdur.coup[[3]][2, ]
predy.in <- cbind(predy.in[dur.coup$end.spell==1], dur.coup$failure[dur.coup$end.spell==1])
png(file='graphics/sepplot_coup.png', width=1024, height=512)
separationplot(predy.in[,1],predy.in[,2],
               shuffle=T, heading='', show.expected=T, newplot=F, 
               type='line', lwd1=5, lwd2=2)
dev.off()

## Contingency table of predicted/observed
predy.in.table <- data.frame(phat=predy.in[,1], y=predy.in[,2])
predy.in.table$yhat <- as.numeric(predy.in.table[, 'phat'] > 0.5)
predy.in.table <- cbind(predy.in.table, 
                        country=dur.coup$country[dur.coup$end.spell==1], 
                        date=dur.coup$date[dur.coup$end.spell==1])

# Classification performance
with(predy.in.table, table(y, yhat))

# Outlier false negative
predy.in.table[with(predy.in.table, { y==1 & yhat==0 }), ]

# False positives
predy.in.table[with(predy.in.table, { y==0 & yhat==1 }), ]

# Watchlist
watchlist <- subset(predy.in.table, date==max(date) & y==0, select=c('country', 'phat'))
watchlist <- watchlist[with(watchlist, order(phat, country, decreasing=T)), ]
watchlist$phat <- round(watchlist$phat, digits=4)
watchlist[watchlist$phat>=0.9, ]