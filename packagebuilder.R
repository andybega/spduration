##
##    Develop, test, and build spduration package
##    Andreas Beger
##    7 October 2014
##

library(devtools)

setwd("~/Work/spduration")

# One time calls during creation
#create("path/to/spduration")
#use_rstudio()
#use_testthat()


#
# The basic steps with devtools are:
# 
#   load_all()   This will simulate a library() call for the package
#   document()   Update namespace and documentation
#   test()       Run unit tests
#   check()      Simulates running R check
#   build()      Build package tarball
#   build_win()  Windows version; Ben will get email when it is done

# While playing around with functions and documentation.
# May have to delete the NAMESPACE file before loading
devtools::load_all(reset=TRUE)

# Document, namespace
devtools::document()

# Run unit tests
# these are in tests/testthat
devtools::test()

# Simulated R check
devtools::check()


# Recreate data, example model, indices -----------------------------------


# Write 00Index for demo and data
rewrite_indices <- function() {
  # for demos
  file.name <- "demo/00Index"
  file.text <- paste0("coups    Modeling P&T 1979-2010 coups")
  write(file.text, file=file.name)
  
  # for data
  file.name <- "data/00Index"
  file.text <- paste0("coups    Powell & Thyne coup data 1979 to 2010 with Polity regime data\n",
                      "model.coups     Estimated example model")
  write(file.text, file=file.name)
}

# will estimate and save example model 
# included in package to save time in code examples
example_model <- function() {
  dur.coup <- add_duration(coups, "succ.coup", unitID='gwcode', tID='year',
                            freq="year")
  model.coups <- spdur(duration ~ polity2, atrisk ~ polity2, data=dur.coup)
  
  # logLike should be: 319.809
  LL_check <- isTRUE(all.equal(as.numeric(logLik(model.coups)), -319.8094, tolerance=1e-07))
  if (!LL_check) {
    warning("Unexpected example model log likelihood, has the estimation function changed?")
  }
  
  save(model.coups, file="data/model.coups.rda")
  return(invisible(NULL))
}

#example_model()
#rewrite_indices()


# Build package (source and Win) ------------------------------------------

#  Manually do:
#   1. Update version and date in "DESCRIPTION"
#   2. Update "NEWS"
#   3. Update version and date in R/spduration-package.R
#

pack_version <- "0.12.1"
pack_path <- "~/Dropbox/Work/spduration"  # directory to which to build pack.

devtools::build(path=pack_path)
devtools::build_win()


# Release to CRAN ---------------------------------------------------------
#
#    Eventually... first add cran-comments.md
#    Send to CRAN
#

#devtools::release()


# Manual package install and check ----------------------------------------

pack_loc <- file.path(pack_path, "spduration_", pack_version, ".tar.gz")
install.packages(pack_loc, type='source', repos="http://cran.rstudio.com")

## Restart R ##

library(spduration)
demo(coups)

##End