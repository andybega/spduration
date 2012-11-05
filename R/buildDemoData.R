## Build test data from ICEWS. Just for documentation, probably no need to run this again.

# data from icews, modify
library(wicews)
data(icews)
icews$low_intensity<-icews$ins.l.count.both.l1+icews$reb.l.count.both.l1+icews$eth.rel.l.count.l1
icews$high_intensity<-icews$ins.h.count.both.l1+icews$reb.h.count.both.l1+icews$eth.rel.h.count.l1
icews$high_neighbors <- icews$W.knn4.std.ins.h.count.both.l1+icews$W.knn4.std.reb.h.count.both.l1+icews$W.knn4.std.eth.rel.h.count.l1
icews$high_neighborhood<-icews$W.centdist.std.ins.h.count.both.l1+icews$W.centdist.std.reb.h.count.both.l1+icews$W.centdist.std.eth.rel.h.count.l1
icews$lgdppc.l1<-log(icews$NY.GDP.PCAP.KD.l1)
insurgency <- subset(icews, select=c('ccode', 'country', 'date', 'insurgency', 'low_intensity', 'high_intensity', 'high_neighborhood', 'high_neighbors', 'lgdppc.l1', 'exclpop.l1', 'excl_groups_count.l1'))
save(insurgency, file='data/insurgency.rda')