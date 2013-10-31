##
##    Build spduration package
##    Andreas Beger
##    21 October 2013
##

rm(list = ls())

# AB: 6.5.2013 starting to transition to devtools/roxygen2
library(devtools)
library(roxygen2)
library(plyr)
library(corpcor)

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
##    3. Update man/spduration-package.Rd
##

# Create package directory on dropbox and copy git package to it
dir.create(pack_db)
file.copy(from=pack_git, to=pack_db, recursive=T)

########
## 3. ##
########
## Estimate and save demo model
##

setwd(paste0(pack_db, "/spduration"))

# Load all functions
load_all("./")

# Demo script and data
load('data/coups.rda')

# Demo model to save estimation for examples
dur.coup <- buildDuration(coups, "succ.coup", unitID='gwcode', tID='year',
                          freq="year")

## Split duration model of coups
model.coups <- spdur(duration ~ polity2, atrisk ~ polity2, data=dur.coup)
rm(dur.coup)
# log-l should be 249.807508 (old) new ll function: 315.591765
# latest: 319.809

# save model to data
save(model.coups, file=paste0(pack_db, "/spduration/data/model.coups.rda"))

########
## 4. ##
########
## Create documentation and 00Index for demo
##

# Build package documentation and namespace
document(roclets=c("namespace", "rd"), reload=T)

# Write 00Index for demo
file.name <- paste0(pack_db, "/spduration/demo/00Index")
file.text <- "coups   Split-duration model of Powell coups.\n"
write(file.text, file=file.name)

########
## 5. ##
########
## This will build and test the package using Terminal:

setwd(pack_db)
system("find . -name '*.DS_Store' -type f -delete")  # delete .DS_Store
system("R CMD build --resave-data spduration")
system("R CMD check spduration")

########
## 5. ##
########
## Install and test package
##
install.packages(paste0(pack_db, "/spduration_", pack_ver, ".tar.gz"), 
                 repos=NULL, type='source')

## Restart R

library(spduration)
demo(coups

########
## 6. ##
########
## Build a Windows version of the package. Go to the following URL and follow 
## the instructions.
## http://win-builder.r-project.org/
##
