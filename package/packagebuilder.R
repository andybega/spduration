##
##    Develop, test, and build spduration package
##    Andreas Beger
##    7 October 2014
##

library(devtools)

setwd("~/Work/spdur_package/package/spduration")

# One time calls during creation
#create("path/to/spduration")
#use_rstudio()

# Package dependencies
#use_testthat()


#
# The basic steps with devtools are:
# 
#   load_all()   This will simulate a library() call for the package
#   document()   Update namespace and documentation
#   check()      Check examples, docs, etc.
#   build()      Build package tarball
#   build_win()  Windows version; Ben will get email when it is done




# While playing around with functions and documentation.
# May have to delete the NAMESPACE file before loading
devtools::load_all(reset=TRUE)

# Document, namespace
devtools::document()

# Run unit tests
devtools::test()

# Write 00Index for demo
file.name <- "demo/00Index"
file.text <- paste0("crime    someting description")
write(file.text, file=file.name)

# Update NEWS file
#   go to knoxr/NEWS
#   manually add any new items

check()

##
##    Build packages (source and Win)
##

#  Manually do:
#   1. Update version and date in "DESCRIPTION"
#   2. Update "NEWS"
#   3. Update version and date in R/spduration-package.R
#

build()
build_win()

##
##    Eventually... first add cran-comments.md
##    Send to CRAN
##

#release()




##
## 		old way
##




##
##    Build spduration package
##    Andreas Beger
##    21 October 2013
##


########
## 1. ##
########
##
## change version here, and working directories
##
pack_ver <- "0.12"

# Working directory from which to source functions, data, etc.
# And location of package on dropbox (which we will create in a sec.)
pack_git <- "~/Work/spdur_package/package/spduration"
pack_db <- "~/Dropbox/Work/spdur_package"

  
########
## 2. ##
########
##
## On github:
##    1. Update "DESCRIPTION"
##    2. Update "NEWS"
##    3. Update man/spduration-package.Rd
##


########
## 3. ##
########
## Estimate and save demo model
##

setwd(pack_git)

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
# latest: 319.809

# save model to data
save(model.coups, file="data/model.coups.rda")

########
## 4. ##
########
## Create documentation and 00Index for demo
##

# Build package documentation and namespace
document(roclets=c("namespace", "rd"), reload=T)

# Write 00Index for demo
file.name <- "demo/00Index"
file.text <- "coups   Split-duration model of Powell coups.\n"
write(file.text, file=file.name)

########
## 5. ##
########
## This will build and test the package using Terminal:

setwd("..")
system("find . -name '*.DS_Store' -type f -delete")  # delete .DS_Store
system("find . -name '.Rapp.history' -type f -delete")
system("R CMD build --resave-data spduration")
system("R CMD check spduration")

# Copy package to dropbox for sharing
file.copy(
	from=paste0("spduration_", pack_ver, ".tar.gz"),
	to=paste0(pack_db, "/spduration_", pack_ver, ".tar.gz"),
	overwrite=TRUE)


########
## 5. ##
########
## Install and test package
##
install.packages(paste0(pack_db, "/spduration_", pack_ver, ".tar.gz"), 
	type='source')

## Restart R

library(spduration)
demo(coups)

########
## 6. ##
########
## Build a Windows version of the package. Go to the following URL and follow 
## the instructions.
## http://win-builder.r-project.org/
##
