##    Demo/test code for "spduration" R package using Powell & Thyne coups
##    Andreas Beger
##    Cassy Dorff
##    October 2013

#This code runs through what is in the demo code for a spdur model
#in the ICEWS package. 
#As is, nothing works. Everything throws 
# Error: 'plot' is not an exported object from 'namespace:spduration'


library(CRISP)
#library(spduration)

data(crisp.data)
data(cutoffs)

##########
# Data prep
#
##########

duration.erv <- spduration::build.duration(data=crisp.data, y="erv", cutoffs$trainingend, 
                            cutoffs$teststart, cutoffs$dataend)

train <- duration.erv$training
test <- duration.erv$test
pred.data <- duration.erv$pred.data
train$lgdpc.l1<-log(train$NY.GDP.PCAP.KD.l1)
train$lpop.l1<-log(train$SP.POP.TOTL.l1)
train$elect<-log(train$ProxElection+1)
train$cut1<-log(train$eth.rel.h.count.l1+1)
train$cut11<-log(train$eth.rel.h.count.l2+1)
train$cut2<-log(train$eth.rel.l.count.l1+1)
train$cut22<-log(train$eth.rel.l.count.l2+1)
train$low_intensity <- train$ins.l.count.both.l1+train$reb.l.count.both.l1+train$eth.rel.l.count.l1
train$high_intensity<-train$ins.h.count.both.l1+train$reb.h.count.both.l1+train$eth.rel.h.count.l1
train$high_neighbors <- train$W.knn4.std.ins.h.count.both.l1+train$W.knn4.std.reb.h.count.both.l1+train$W.knn4.std.eth.rel.h.count.l1
train$high_neighborhood<-train$W.centdist.std.ins.h.count.both.l1+train$W.centdist.std.reb.h.count.both.l1+train$W.centdist.std.eth.rel.h.count.l1
test$lgdpc.l1<-log(test$NY.GDP.PCAP.KD.l1)
test$lpop.l1<-log(test$SP.POP.TOTL.l1)
test$elect<-log(test$ProxElection+1)
test$cut1<-log(test$eth.rel.h.count.l1+1)
test$cut11<-log(test$eth.rel.h.count.l2+1)
test$cut2<-log(test$eth.rel.l.count.l1+1)
test$cut22<-log(test$eth.rel.l.count.l2+1)
test$low_intensity<-test$ins.l.count.both.l1+test$reb.l.count.both.l1+test$eth.rel.l.count.l1
test$high_intensity<-test$ins.h.count.both.l1+test$reb.h.count.both.l1+test$eth.rel.h.count.l1
test$high_neighbors <- test$W.knn4.std.ins.h.count.both.l1+test$W.knn4.std.reb.h.count.both.l1+test$W.knn4.std.eth.rel.h.count.l1
test$high_neighborhood<-test$W.centdist.std.ins.h.count.both.l1+test$W.centdist.std.reb.h.count.both.l1+test$W.centdist.std.eth.rel.h.count.l1
pred.data$lgdpc.l1<-log(pred.data$NY.GDP.PCAP.KD.l1)
pred.data$lpop.l1<-log(pred.data$SP.POP.TOTL.l1)
pred.data$elect<-log(pred.data$ProxElection+1)
pred.data$cut1<-log(pred.data$eth.rel.h.count.l1+1)
pred.data$cut11<-log(pred.data$eth.rel.h.count.l2+1)
pred.data$cut2<-log(pred.data$eth.rel.l.count.l1+1)
pred.data$cut22<-log(pred.data$eth.rel.l.count.l2+1)
pred.data$low_intensity<-pred.data$ins.l.count.both.l1+pred.data$reb.l.count.both.l1+pred.data$eth.rel.l.count.l1
pred.data$high_intensity<-pred.data$ins.h.count.both.l1+pred.data$eth.rel.h.count.l1+pred.data$reb.h.count.both.l1
pred.data$high_neighbors <- pred.data$W.knn4.std.ins.h.count.both.l1+pred.data$W.knn4.std.reb.h.count.both.l1+pred.data$W.knn4.std.eth.rel.h.count.l1
pred.data$high_neighborhood<-pred.data$W.centdist.std.ins.h.count.both.l1+pred.data$W.centdist.std.reb.h.count.both.l1+pred.data$W.centdist.std.eth.rel.h.count.l1
 
model <- spduration::spdur(
duration ~ exclpop.l1 + high_neighbors + high_intensity + low_intensity + high_neighborhood + elect, 
+   c ~ excl_groups_count + DEMOC.l1 + Amnesty.l1 + lgdpc.l1 + SH.DYN.MORT.l1,
+   last=train$end.spell, data=train, test=test, distr="weibull", iter=300
+   )

pred.probs <- spduration::predict(model, pred.data)

pr.nc.in <- cbind(train$erv,pred.probs$pr.in$n.cure.t.in)

pr.nc.out <- cbind(test$erv,pred.probs$pr.out$n.cure.t.out)

pr.ht.in <- cbind(train$erv,pred.probs$pr.in$pr.c.h.in)

pr.ht.out <- cbind(test$erv,pred.probs$pr.out$pr.c.h.out)

# Separation plots
par(mfrow=c(2,2), mar=c(1,1,2,1))

spduration::plot(pr.nc.in[,2], pr.nc.in[,1], shuffle=T, heading="Pr(Non immunity), In-sample", show.expected=T, newplot=F)

spduration::plot(pr.nc.out[,2], pr.nc.out[,1], shuffle=T, heading="Pr(Non immunity), Out-of-sample", show.expected=T, newplot=F)

spduration::plot(pr.ht.in[,2], pr.ht.in[,1], shuffle=T, heading="Pr(Ethnic Violence at t | Non immunity), In-sample", show.expected=T, newplot=F)

spduration::plot(pr.ht.out[,2], pr.ht.out[,1], shuffle=T, heading="Pr(Ethnic Violence at t | Non immunity), Out-of-sample", show.expected=T, newplot=F)

##########
# Forecast and prediction tables
#
##########

insample.table.split <- data.frame(ccode = train$ccode[train$end.spell==1],
                    date = train$date[train$end.spell==1],
                    in.pr.non.immunity = round(pred.probs$pr.in$n.cure.t.in[train$end.spell==1],4)
                    in.pr.fail = round(pred.probs$pr.in$pr.c.h.in[train$end.spell==1],4))

outsample.table.split <- data.frame(ccode = test$ccode[test$end.spell==1],
                   date = test$date[test$end.spell==1],
                   out.pr.non.immunity = round(pred.probs$pr.out$n.cure.t.out[test$end.spell==1],4),
                   out.pr.fail = round(pred.probs$pr.out$pr.c.h.out[test$end.spell==1],4))

# Pr(non immunity)
predict.table.split.pr.non.immunity <- round(pred.probs$pred.tbl.ncure,4)

# Pr(event | non immunity)
predict.table.split.pr.event <- round(pred.probs$pred.tbl.fail,4)

