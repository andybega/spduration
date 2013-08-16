##########
# Build spdur R package
# November 2012
# Andreas Beger
#
###########

rm(list = ls())

# AB: 6.5.2013 starting to transition to devtools/roxygen2
library(devtools)
library(roxygen2)

########
## 1. ##
########
##
## change version here, and workind directories
##
pack_ver <- "0.10"

# Working directory from which to source functions, data, etc.
# And location of package on dropbox (which we will create in a sec.)
if(Sys.info()["user"]=="ab428") {
  pack_git <- "~/Work/spdur_package/package/spduration"
  pack_db <- paste0("~/Dropbox/Work/spdur_package/package/spduration_", 
                    pack_ver)
} else if(Sys.info()["user"]=="adbeger") {
  pack_git <- "~/Work/spdur_package/package/spduration"
  pack_db <- paste0("~/Dropbox/Work/spdur_package/package/spduration_", 
                    pack_ver)
}
  
########
## 2. ##
########
##
## On github:
##    1. Update "DESCRIPTION"
##    2. Update "NEWS"
##

# Create package directory on dropbox and copy git package to it
dir.create(pack_db)
file.copy(from=pack_git, to=pack_db, recursive=T)

########
## 3. ##
########
## Source functions, data, demo, etc. and create manuals
##

setwd(paste0(pack_db, "/spduration"))

# Core functionality
source('R/spdur.R')
source('R/spweibull.R')
source('R/sploglog.R')
source('R/expand.call.R')

# Duration build function
source('R/buildDuration.R')
source('R/panelLag.R')

# Demo script and data
load('data/insurgency.rda')

# Demo model to save estimation for examples
duration.ins <- buildDuration(insurgency, 'insurgency', unitID='ccode', tID='date')
duration.ins <- duration.ins[!is.na(duration.ins$failure), ]

## Split duration model of insurgency
model.ins <- spdur(
  duration ~ low_intensity + high_neighbors + exclpop.l1,
  atrisk ~ excl_groups_count.l1 + high_neighborhood + high_intensity + exclpop.l1 + lgdppc.l1,
  last='end.spell', data=duration.ins, distr="weibull", max.iter=300)
rm(duration.ins)
# log-l should be 249.807508 (old) new ll function: 315.591765

## Build package documentation and package
document(roclets=c("namespace", "rd"), reload=T)

########
## 4. ##
########
## This will build and test the package using Terminal:
setwd(pack_db)
system('R CMD build spduration')
system('R CMD check spduration')

########
## 5. ##
########
## Install and test package
##
install.packages(paste0(pack_db, paste0("spduration_", pack_ver, ".tar.gz"), 
                 repos=NULL, type='source'))

## Restart R

library(spduration)
demo(insurgency)

########
## 6. ##
########
## Build a Windows version of the package. Go to the following URL and follow 
## the instructions.
## http://win-builder.r-project.org/
##
