##########
# What: Test code for spdur dev
# Date: November 2012
# Who:  Andreas Beger
###########

rm(list=ls())

source('R/spdur.R')

library(spdur)

load('delete/dur.coup.RData')

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

## Split duration model
model <- 
  spdur(duration ~ d.major.l1 + d.minor.l1 + ln.dom.cris.i.count.l1 + ln.ProxElection.l1,
        atrisk ~ ln.gdppc.l1 + med.polity.l1 + ln.dom.cris.i.count.l1 + ln.ProxElection.l1,
        data=dur.coup, last=dur.coup$end.spell, distr='weibull', max.iter=200)


# Separationplot
predy.in <- spdur.csp[[3]][2, ]
predy.in <- cbind(predy.in[dur.csp$end.spell==1], dur.csp$failure[dur.csp$end.spell==1])
png(file='graphics/sepplot_CSP_coup.png', width=1024, height=512)
separationplot(predy.in[,1],predy.in[,2],
               shuffle=T, heading='', show.expected=T, newplot=F, 
               type='line', lwd1=5, lwd2=2)
dev.off()