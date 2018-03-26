# spduration 0.17.1 (2018-03-26)

* Add pkgdown static documentation website at https://andybeger.com/spduration.
* Fix C++ code that was causing warnings on R-devel; replacing & and | with && and ||. 

# spduration 0.17.0 (2017-10-04)

* Adds proper handling of the various `na.action` options for `spdur` and its `predict`, `residuals`, and `fitted` methods. 
* Import `forecast` generic function from `forecast` package rather than re-defining it. Adds `forecast` to neccessary imports. 
* Added bug reports link to DESCRIPTION for github issues page (https://github.com/andybega/spduration/issues). 
* Fixes minor errors in introduction vignette and data documentation. 
* Register C++ routines to avoid R-devel note.

# spduration 0.16.0 (2017-04-11)

* separationplot.spdur -> sepplot. `separationplot` is a standalone function so change the `spdur` version to a simple wrapper. 
* Cleaned up NAMESPACE. Moved `stats` to imported package in description and removed all associated explicit namespace generic imports. 
* Added package vignette.
* Added file `init.c` with calls to `R_registerRoutines()` and `R_useDynamicSymbols()`; also use `.registration=TRUE` in `useDynLib` in `NAMESPACE`. R-devel (and R 3.4.0 in the future) `R CMD check` issues a NOTE for registration of routines, this is to avoid that note.  
* Updated DESCRIPTION and added references.

# spduration 0.15.1 (2016-05-12)

* Fixes bug in `summary.spdur` that would return wrong estimates for `log(alpha)`.
* Better column names for `summary` and `xtable` methods.
* Fixes test error after `testthat` update.

# spduration 0.15.0

* Added accessor methods for `terms`, `model.matrix`, `coef`, and `vcov`. 
* Added `fitted` and `residuals` methods.
* Fixes issue #17, which led to errors in `summary.spdur` when called on a model 
with factor variables or without intercept terms. 
* Fixes an issue with the hazard rate plot confidence intervals (`plot_hazard(ci = TRUE)`), where CIs coud be wrong because coefficients were sampled by equation rather than using the full variance covariance matrix.

# spduration 0.14.0

* Added Belkin & Schofer 2003 coup data in `data(bscoup)`, see `?bscoup`. 
* Replaced `plot_hazard1` and `plot_hazard2` with `plot_hazard`, added support for loglog models, and other internal streamlining of plotting code. 
* Partial fix for summarizing and printing results for models without intercept term.
* Removed documentation and export for several internal functions.

# spduration 0.13.1

* Initial CRAN release
