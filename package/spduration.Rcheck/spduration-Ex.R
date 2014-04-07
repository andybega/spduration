pkgname <- "spduration"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('spduration')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("AIC.spdur")
### * AIC.spdur

flush(stderr()); flush(stdout())

### Name: AIC.spdur
### Title: AIC method for spdur
### Aliases: AIC.spdur

### ** Examples

data(model.coups)
AIC(model.coups)



cleanEx()
nameEx("BIC.spdur")
### * BIC.spdur

flush(stderr()); flush(stdout())

### Name: BIC.spdur
### Title: BIC method for spdur
### Aliases: BIC.spdur

### ** Examples

data(model.coups)
BIC(model.coups)



cleanEx()
nameEx("buildDuration")
### * buildDuration

flush(stderr()); flush(stdout())

### Name: buildDuration
### Title: Build duration version of panel data
### Aliases: buildDuration

### ** Examples

# Yearly data
data <- data.frame(y=c(0,0,0,1,0),
                   unitID=c(1,1,1,1,1),
                   tID=c(2000, 2001, 2002, 2003, 2004))
dur.data <- buildDuration(data, "y", "unitID", "tID", freq="year")
dur.data



cleanEx()
nameEx("coups")
### * coups

flush(stderr()); flush(stdout())

### Name: coups
### Title: Global coups, 1979 to 2010
### Aliases: coups

### ** Examples

data(coups)
table(coups$succ.coup)



cleanEx()
nameEx("forecast.spdur")
### * forecast.spdur

flush(stderr()); flush(stdout())

### Name: forecast.spdur
### Title: Plot spdur object predictions
### Aliases: forecast.spdur

### ** Examples

data(coups)
data(model.coups)

coups.dur <- buildDuration(coups, "succ.coup", "gwcode", "year", freq="year")
pred.data <- coups.dur[coups.dur$year==max(coups.dur$year), ]
pred.data <- pred.data[complete.cases(pred.data), ]
fcast <- forecast(model.coups, pred.data=pred.data)



cleanEx()
nameEx("logLik.spdur")
### * logLik.spdur

flush(stderr()); flush(stdout())

### Name: logLik.spdur
### Title: Log-Likelihood of an spdur Object
### Aliases: logLik.spdur

### ** Examples

data(model.coups)
logLik(model.coups)



cleanEx()
nameEx("model.coups")
### * model.coups

flush(stderr()); flush(stdout())

### Name: model.coups
### Title: Model of global coups from 1979 to 2010
### Aliases: model.coups

### ** Examples

data(model.coups)
str(model.coups)



cleanEx()
nameEx("panelLag")
### * panelLag

flush(stderr()); flush(stdout())

### Name: panelLag
### Title: Lag panel data
### Aliases: panelLag

### ** Examples

data(coups)
# No need to order before using panelLag, just do it here so we can compare results below.
coups <- coups[order(coups$gwcode, coups$year), ]
test <- panelLag("polity2", "gwcode", "year", data=coups)

# Compare output
head(coups$polity2)
head(test)



cleanEx()
nameEx("plot.spdur")
### * plot.spdur

flush(stderr()); flush(stdout())

### Name: plot.spdur
### Title: Plot split-duration model fit.
### Aliases: plot.spdur

### ** Examples

# get model estimates
data(model.coups)

# plot
p <- plot(model.coups)



cleanEx()
nameEx("predict.spdur")
### * predict.spdur

flush(stderr()); flush(stdout())

### Name: predict.spdur
### Title: Predict fitted values for a split-population duration model
### Aliases: predict.spdur

### ** Examples

# get model estimates
data(model.coups)
atrisk <- predict(model.coups)



cleanEx()
nameEx("print.summary.spdur")
### * print.summary.spdur

flush(stderr()); flush(stdout())

### Name: print.summary.spdur
### Title: Print a split-population duration model results summary
### Aliases: print.summary.spdur

### ** Examples

data(model.coups)
s <- summary(model.coups)
class(s)
print(s)



cleanEx()
nameEx("spdur")
### * spdur

flush(stderr()); flush(stdout())

### Name: spdur
### Title: Split-population duration (cure) regression
### Aliases: spdur

### ** Examples

## Not run: 
##D # Prepare data
##D data(coups)
##D dur.coups <- buildDuration(coups, "succ.coup", unitID="gwcode", tID="year",
##D                            freq="year")
##D 
##D # Estimate model
##D model.coups <- spdur(duration ~ polity2, atrisk ~ polity2, data=dur.coups)
## End(Not run)



cleanEx()
nameEx("spdurCrisp")
### * spdurCrisp

flush(stderr()); flush(stdout())

### Name: spdurCrisp
### Title: Split-Pop Duration Model Wrapper for CRISP/ICEWS
### Aliases: spdurCrisp

### ** Examples

## Not run: 
##D # Prepare data
##D data(coups)
##D dur.coups <- buildDuration(coups, "succ.coup", unitID="gwcode", tID="year",
##D                            freq="year")
##D 
##D # Estimate model
##D model3 <- spdurCrisp(
##D   duration ~ polity2,
##D   atrisk ~ polity2,
##D   train=dur.coups, test=dur.coups[1,],
##D   pred=dur.coups[1,])
## End(Not run)



cleanEx()
nameEx("spduration-package")
### * spduration-package

flush(stderr()); flush(stdout())

### Name: spduration-package
### Title: Split-Population Duration (Cure) Regression Models
### Aliases: spduration-package spduration

### ** Examples

library(spduration)
demo(coups)



cleanEx()
nameEx("summary.spdur")
### * summary.spdur

flush(stderr()); flush(stdout())

### Name: summary.spdur
### Title: Summarize split-population duration results
### Aliases: summary.spdur

### ** Examples

data(model.coups)
s <- summary(model.coups)
class(s)
print(s)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
