# Profile build duration

library(plyr)
library(profr)
library(spduration)

data <- data.frame(
	unitID=as.character(rep(1:365, each=1000)),
	time=rep(seq.Date(as.Date("2000-01-01"), by="day", length.out=365), 1000),
	y=rbinom(365000, size=1, p=0.01),
	stringsAsFactors=FALSE)

meth1 <- function(data) {
	res <- data
	res <- ddply(res, .variables="unitID", transform, end=max(time))
	res
}

p <- profr(test <- meth1(data), 0.05)
# runs in 5.6s

meth2 <- function(data) {
	res <- data
	res$end <- unlist(by(res[, "time"], res[, "unitID"], max))
	origin <- as.Date("1970-01-01")
	res$end <- as.Date(res$end, origin=origin)
	res
}

p2 <- profr(test2 <- meth2(data), 0.05)
# runs in 0.5s

# source buildDuration

# with old buildDuration (ddply for end spell marker)
p3 <- profr(test3 <- spduration::buildDuration(data, "y", "unitID", "time", 
			sort=FALSE, ongoing=FALSE), 0.05)
# 17.45s

# with new buildDuration (by for end spell marker)
p4 <- profr(test4 <- buildDuration(data, "y", "unitID", "time",
			sort=FALSE, ongoing=FALSE), 0.05)
# 8.35s


# original profile 
               f level   time  start    end  leaf      source
8  buildDuration     1 3878.5    0.0 3878.5 FALSE  spduration
9          is.na     2   11.0    0.0   11.0 FALSE        base
10           $<-     2    0.5   11.5   12.0 FALSE        base
11             [     2    3.5   12.0   15.5 FALSE        base
12           $<-     2    0.5   15.5   16.0 FALSE        base
13         ddply     2 3744.5   16.5 3761.0 FALSE        plyr
14             [     2    0.5 3761.0 3761.5 FALSE        base
15        format     2   12.0 3761.5 3773.5 FALSE RPostgreSQL
16        ifelse     2    1.5 3773.5 3775.0  TRUE        base
17           $<-     2    0.5 3775.0 3775.5 FALSE        base
18        ifelse     2    1.0 3775.5 3776.5  TRUE        base
19           $<-     2    0.5 3777.0 3777.5 FALSE        base
20        ifelse     2    1.0 3777.5 3778.5  TRUE        base

# profile with by instead of ddply
               f level  time start   end  leaf      source
8  buildDuration     1 366.5   0.0 366.5 FALSE        <NA>
9          is.na     2   5.0   0.0   5.0 FALSE        base
10             [     2   3.5   5.5   9.0 FALSE        base
11           $<-     2   1.0   9.0  10.0 FALSE        base
12        unlist     2 240.5  10.0 250.5 FALSE        base
13       as.Date     2   0.5 250.5 251.0 FALSE        base
14           $<-     2   0.5 251.0 251.5 FALSE        base
15        format     2  11.0 251.5 262.5 FALSE RPostgreSQL
16        ifelse     2   2.0 262.5 264.5  TRUE        base
