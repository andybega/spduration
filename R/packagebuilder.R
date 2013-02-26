##########
# Build spdur R package
# November 2012
# Andreas Beger
#
###########

rm(list = ls())

# Package folder name

###
###
### change version here
###
###
package.name <- 'spduration_0.9'

# Directory in which to create package
if(Sys.info()["user"]=="ab428") package.path <- paste('~/Dropbox/Work/spdur_package/package', package.name, sep='/')

# Working directory from which to source functions, data, etc.
if(Sys.info()["user"]=="ab428") setwd('~/Work/spdur_package')

########
## 1. ##
########
## Source functions, data, demo, etc. and build package skeleton
##

# Core functionality
source('R/functions/spdur.R')
source('R/functions/spweibull.R')
source('R/functions/sploglog.R')
source('R/functions/predict.spdur.R')
source('R/functions/plot.spdur.R')
source('R/functions/forecast.R')
source('R/functions/expand.call.R')

# Duration build function
source('R/functions/buildDuration.R')
source('R/functions/panelLag.R')

# Demo script and data
load('data/insurgency.rda')

# Demo model to save estimation for examples
duration.ins <- buildDuration(insurgency, 'insurgency', unitID='ccode', tID='date')

## Split duration model of insurgency
model.ins <- spdur(
  duration ~ low_intensity + high_neighbors + exclpop.l1,
  atrisk ~ excl_groups_count.l1 + high_neighborhood + high_intensity + exclpop.l1 + lgdppc.l1,
  last='end.spell', data=duration.ins, distr="weibull", max.iter=300)
rm(duration.ins)
# log-l should be 249.807508 (old) new ll function: 

# Create/navigate to package directory
if (file.exists(package.path)) {
  setwd(package.path) 
  } else {
  dir.create(file.path(package.path))
  setwd(package.path)
}

# Delete if package folder already exists
if (file.exists('spduration')) unlink('spduration', recursive=T)

# Build package skeleton
rm(package.name, package.path) 
package.skeleton('spduration')

########
## 2. ##
########
## Copy and paste from github to package directory.
##
## 1. On the github folder, update the version in "DESCRIPTION" and package 
##    manual file, update "NEWS".
## 2. Copy & paste from github to dropbox:
##        "DESCRIPTION"
##        "NAMESPACE"
##        "NEWS"
##        "man" folder
##        "demo" folder
##

########
## 3. ##
########
## This will build the package using Terminal:
system('cd ~/Dropbox/Work/spdur_package/package/spduration_0.9')
system('R CMD build spduration')

########
## 4. ##
########
## On the terminal, check if the package is alright.
system('cd ~/Dropbox/Work/spdur_package/package/spduration_0.9')
system('R CMD check spduration')

########
## 5. ##
########
## Install and test package
##
install.packages('~/Dropbox/Work/spdur_package/package/spduration_0.8/spduration_0.8.tar.gz', 
                 repos=NULL, type='source')

## Restart R

library(spduration)
demo(insurgency)

########
## 5. ##
########
## Build a Windows version of the package. Go to the following URL and follow the instruction.
## http://win-builder.r-project.org/
##
