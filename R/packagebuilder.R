##########
# Build spdur R package
# November 2012
# Andreas Beger
#
###########

rm(list = ls())

# Package folder name
package.name <- 'spdur_0.2'

# Directory in which to create package
if(Sys.info()["user"]=="adbeger") package.path <- paste('~/Dropbox/Research/spdur_package/package', package.name, sep='/')

# Working directory from which to source functions, data, etc.
if(Sys.info()["user"]=="adbeger") setwd('~/Research/spdur_package')

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

# Duration build function
source('R/functions/buildDuration.R')
source('R/functions/panelLag.R')

# Demo script and data
load('delete/dur.coup.RData')

# Create/navigate to package directory
if (file.exists(package.path)) {
  setwd(package.path) 
  } else {
  dir.create(file.path(package.path))
  setwd(path.package)
}

# Delete if package folder already exists
if (file.exists('spdur')) unlink('spdur', recursive=T)

# Build package skeleton
rm(package.name, package.path)
package.skeleton('spdur')

########
## 2. ##
########
## Go into 
##   Dropbox/spdur_package/package/spdur_X.X/
##
## Copy & pastes the following objects from the githubprevious version of the
## package ("packageX.X-1.X/CRISP/") into the latest version of the package 
## ("packageX.X.X/CRISP/"). You will have to overwrite (replace) the ones that
## already exist in the folder of the latest version of the package. 
##
##  1. "DESCRIPTION" file
##  2. "NAMESPACE"
##
## Open the "DESCRIPTION" file, and modify the version information, date, etc.
##
## Copy and paste the following two folders from github to 
## ("packageX.X.X/CRISP/")
## 
## 1. "man" folder to "man" folder
## 2. "demo" folder on git to "demo" folder
##

########
## 3. ##
########
## This will build the package using Terminal:
system('cd ~/Dropbox/Research/spdur_package/package/spdur_0.1')
system('R CMD build spdur')

########
## 4. ##
########
## On the terminal, check if the package is alright.
system('cd ~/Dropbox/Research/spdur_package/package/spdur_0.1')
system('R CMD check spdur')


########
## 5. ##
########
## Build a Windows version of the package. Go to the following URL and follow the instruction.
## http://win-builder.r-project.org/
##
